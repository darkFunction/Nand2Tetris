import Foundation

enum Segment: String {
    case local
    case argument
    case this
    case that
    case pointer
    case staticSeg = "static"
    case temp
    case constant
}

enum ArithmeticOperation: String {
    case add
    case sub
    case neg
    case eq
    case gt
    case lt
    case and
    case or
    case not
}

enum Command: CustomStringConvertible {
    case push(segment: Segment, index: UInt)
    case pop(segment: Segment, index: UInt)
    case arithmetic(type: ArithmeticOperation)
    
    var description: String {
        switch self {
        case .push(let segment, let index): return "push \(segment) \(index)"
        case .pop(let segment, let index): return "pop \(segment) \(index)"
        case .arithmetic(let operation): return "\(operation)"
        }
    }
}

extension String {
    func stripComments() -> String {
        if let found = range(of: "//") {
            return String(prefix(upTo: found.lowerBound))
        }
        return self
    }
    
    var lines: [String] {
        return split(maxSplits: Int.max, omittingEmptySubsequences: false, whereSeparator: { $0.isNewline }).map(String.init)
    }
}

class Parser {
    private let text: String
    
    init(_ text: String) {
        self.text = text
    }
    
    func parse() throws -> [Command] {
        return try sanitisedLines().map { try parseLine($0) }
    }
    
    struct SyntaxError: LocalizedError {
        let type: SyntaxErrorType
        let lineNumber: Int
        
        enum SyntaxErrorType: CustomStringConvertible {
            case unknown
            case unrecognisedCommand(_ command: String)
            case invalidArgumentCount(_ args: [String], expectedCount: Int)
            case invalidSegment(_ segment: String)
            case invalidIndex(_ index: String)
            
            var description: String {
                switch self {
                case .unknown:
                    return "Unable to parse"
                case .unrecognisedCommand(let command):
                    return "Unrecognised command '\(command)'"
                case .invalidArgumentCount(let args, let expectedCount):
                    return "Invalid number of arguments, expected \(expectedCount), got \(args.count)"
                case .invalidSegment(let segment):
                    return "Unrecognised segment '\(segment)'"
                case .invalidIndex(let index):
                    return "Invalid index '\(index)'"
                }
            }
        }
        
        var errorDescription: String? {
            return "Syntax error on line \(lineNumber): " + String(describing: type)
        }
    }
    
    typealias IndexedLine = (number: Int, text: String)
    
    private func sanitisedLines() -> [IndexedLine] {
        return text.lines.enumerated().compactMap {
            let element = $0.element.stripComments().trimmingCharacters(in: .whitespacesAndNewlines)
            return element.count > 0 ? IndexedLine($0.offset + 1, element) : nil
        }
    }
    
    private func parseLine(_ line: IndexedLine) throws -> Command {
        let args = line.text.components(separatedBy: .whitespaces)
        guard let commandString = args.first else {
            throw SyntaxError(type: .unknown, lineNumber: line.number)
        }
        switch commandString {
        case "push", "pop":
            guard args.count == 3 else {
                throw SyntaxError(type: .invalidArgumentCount(Array(args[1..<args.count]), expectedCount: 2), lineNumber: line.number)
            }
            guard let segment = Segment(rawValue: args[1]) else {
                throw SyntaxError(type: .invalidSegment(args[1]), lineNumber: line.number)
            }
            guard let index = UInt(args[2]) else {
                throw SyntaxError(type: .invalidIndex(args[2]), lineNumber: line.number)
            }
            return args[0] == "push"
                ? .push(segment: segment, index: index)
                : .pop(segment: segment, index: index)
        default:
            guard let operation = ArithmeticOperation(rawValue: args[0]) else {
                throw SyntaxError(type: .unrecognisedCommand(commandString), lineNumber: line.number)
            }
            return .arithmetic(type: operation)
        }
    }
}

enum SegmentMap {
    case indirect(symbol: String)
    case direct(baseIndex: UInt)
    case constant
    case staticVar(baseName: String)
    
    // Fetch address into 'D' register
    func asmSelector(index: UInt) -> String {
        switch self {
        case .indirect(let symbol):
            return """
                @\(symbol)
                D=M
                @\(index)
                A=D+A
            """
        case .constant:
            return """
                @\(index)
            """
        case .direct(let baseIndex):
            return """
                @R\(baseIndex+index)
            """
            
        case .staticVar(let baseName):
            return """
                @\(baseName).\(index)
            """
        }
    }
}

extension Segment {
    func segmentMap(staticName: String = "static") -> SegmentMap {
        switch self {
        case .constant:     return SegmentMap.constant
        case .local:        return SegmentMap.indirect(symbol: "LCL")
        case .argument:     return SegmentMap.indirect(symbol: "ARG")
        case .this:         return SegmentMap.indirect(symbol: "THIS")
        case .that:         return SegmentMap.indirect(symbol: "THAT")
        case .pointer:      return SegmentMap.direct(baseIndex: 3)
        case .temp:         return SegmentMap.direct(baseIndex: 5)
        case .staticSeg:    return SegmentMap.staticVar(baseName: staticName)
        }
    }
}

extension ArithmeticOperation {
    var asm: String {
        switch self {
        case .add:  return "M+D"
        case .sub:  return "M-D"
        case .and:  return "M&D"
        case .or:   return "M|D"
        case .neg:  return "-M"
        case .not:  return "!M"
        case .eq:   return "JEQ"
        case .gt:   return "JGT"
        case .lt:   return "JLT"
        }
    }
}

class CodeWriter {
    let commands: [Command]
    
    init(commands: [Command]) {
        self.commands = commands
    }
    
    func generateCode(outFile: URL) -> String {
        return compile(staticName: outFile.deletingPathExtension().lastPathComponent)
    }
    
    private func compile(staticName: String) -> String {
        return commands.map { "// \($0.description)\n" + translate($0, staticName: staticName ) }.map{ $0+"\n" }.reduce("", +)
    }
    
    private var branchCounter: Int = 0
    
    private let pushDToStack: String = """
            @SP
            A=M
            M=D
            @SP
            M=M+1
        """
    
    private let popStackToD: String = """
            @SP
            M=M-1
            A=M
            A=M
            D=D+A
            A=D-A
            D=D-A
        """
    
    func translate(_ command: Command, staticName: String) -> String {
        switch command {
        case .push(let segment, let index):
            return """
            \(segment.segmentMap(staticName: staticName).asmSelector(index: index))
                \(segment == .constant ? "D=A" : "D=M")
            \(pushDToStack)
            """
        case .pop(let segment, let index):
            return """
            \(segment.segmentMap(staticName: staticName).asmSelector(index: index))
                D=A
            \(popStackToD)
                M=D
            """
        case .arithmetic(let operation):
            switch operation {
            case .not, .neg:
                return """
                    @SP
                    A=M-1
                    M=\(operation.asm)
                """
            case .gt, .lt, .eq:
                branchCounter += 1
                return """
                    @SP
                    AM=M-1
                    D=M
                    A=A-1
                    D=M-D
                    @_IF.\(branchCounter)
                    D;\(operation.asm)
                    @SP
                    A=M-1
                    M=0
                    @_CONT.\(branchCounter)
                    0;JMP
                    (_IF.\(branchCounter))
                    @SP
                    A=M-1
                    M=-1
                    (_CONT.\(branchCounter))
                """
            default:
                return """
                    @SP
                    AM=M-1
                    D=M
                    A=A-1
                    MD=\(operation.asm)
                """
            }
        }
    }
}

extension URL {
    func withExtension(_ ext: String) -> URL {
        return deletingPathExtension().appendingPathExtension(ext)
    }
}

guard CommandLine.arguments.count == 2 else {
    print("Usage: VMTranslator <file>")
    exit(1)
}

let inFileURL = URL(fileURLWithPath: CommandLine.arguments[1])
let parsed: [Command]
do {
    let contents = try String(contentsOf: inFileURL, encoding: .utf8)
    parsed = try Parser(contents).parse()
} catch {
    print(error.localizedDescription)
    exit(1)
}

let writer = CodeWriter(commands: parsed)
print(writer.generateCode(outFile: inFileURL.withExtension("asm")))



