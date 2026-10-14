
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <variant>
#include <stdexcept>
#include <cctype>
#include <cstdlib>

using namespace std;

using Value = variant<double, string>;

enum class Type {
    Number, String, Name,
    Plus, Minus, Star, Slash,
    Equal, LParen, RParen, Comma,
    Newline, End
};

struct Token {
    Type type;
    string text;
};

class Lexer {
    string source;
    size_t pos = 0;

public:
    explicit Lexer(string code) : source(move(code)) {}

    vector<Token> scan() {
        vector<Token> tokens;

        while (pos < source.size()) {
            char c = source[pos];

            if (c == ' ' || c == '\t' || c == '\r') {
                ++pos;
                continue;
            }

            if (c == '#') {
                while (pos < source.size() &&
                       source[pos] != '\n')
                    ++pos;
                continue;
            }

            if (c == '\n' || c == ';') {
                tokens.push_back({Type::Newline, "\\n"});
                ++pos;
                continue;
            }

            if (isdigit(c) || (c == '.' &&
                pos + 1 < source.size() &&
                isdigit(source[pos + 1]))) {
                size_t start = pos;

                while (pos < source.size() &&
                       (isdigit(source[pos]) ||
                        source[pos] == '.'))
                    ++pos;

                tokens.push_back({
                    Type::Number,
                    source.substr(start, pos - start)
                });
                continue;
            }

            if (isalpha(c) || c == '_') {
                size_t start = pos++;

                while (pos < source.size() &&
                       (isalnum(source[pos]) ||
                        source[pos] == '_'))
                    ++pos;

                tokens.push_back({
                    Type::Name,
                    source.substr(start, pos - start)
                });
                continue;
            }

            if (c == '"' || c == '\'') {
                char quote = c;
                ++pos;
                string value;

                while (pos < source.size() &&
                       source[pos] != quote) {
                    char ch = source[pos++];

                    if (ch == '\\' && pos < source.size()) {
                        char escaped = source[pos++];

                        if (escaped == 'n')
                            value += '\n';
                        else if (escaped == 't')
                            value += '\t';
                        else
                            value += escaped;
                    } else {
                        value += ch;
                    }
                }

                if (pos >= source.size())
                    throw runtime_error(
                        "String nao terminada"
                    );

                ++pos;
                tokens.push_back({Type::String, value});
                continue;
            }

            Type type;

            switch (c) {
                case '+': type = Type::Plus; break;
                case '-': type = Type::Minus; break;
                case '*': type = Type::Star; break;
                case '/': type = Type::Slash; break;
                case '=': type = Type::Equal; break;
                case '(': type = Type::LParen; break;
                case ')': type = Type::RParen; break;
                case ',': type = Type::Comma; break;

                default:
                    throw runtime_error(
                        string("Caractere inesperado: ") + c
                    );
            }

            tokens.push_back({type, string(1, c)});
            ++pos;
        }

        tokens.push_back({Type::Newline, "\\n"});
        tokens.push_back({Type::End, ""});

        return tokens;
    }
};

class Interpreter {
    vector<Token> tokens;
    size_t pos = 0;
    unordered_map<string, Value> variables;

    Token current() const {
        return tokens[pos];
    }

    Token advance() {
        return tokens[pos++];
    }

    bool match(Type type) {
        if (current().type != type)
            return false;

        advance();
        return true;
    }

    void expect(Type type, const string& message) {
        if (!match(type))
            throw runtime_error(message);
    }

    double number(const Value& value) {
        if (!holds_alternative<double>(value))
            throw runtime_error("Era esperado um numero");

        return get<double>(value);
    }

    string stringify(const Value& value) {
        if (holds_alternative<string>(value))
            return get<string>(value);

        ostringstream out;
        out << get<double>(value);
        return out.str();
    }

    Value expression() {
        return addition();
    }

    Value addition() {
        Value left = multiplication();

        while (current().type == Type::Plus ||
               current().type == Type::Minus) {
            Type op = advance().type;
            Value right = multiplication();

            if (op == Type::Plus &&
                holds_alternative<string>(left) &&
                holds_alternative<string>(right)) {
                left = get<string>(left) +
                       get<string>(right);
            } else {
                double a = number(left);
                double b = number(right);

                left = op == Type::Plus
                    ? a + b
                    : a - b;
            }
        }

        return left;
    }

    Value multiplication() {
        Value left = unary();

        while (current().type == Type::Star ||
               current().type == Type::Slash) {
            Type op = advance().type;
            double a = number(left);
            double b = number(unary());

            if (op == Type::Slash && b == 0)
                throw runtime_error("Divisao por zero");

            left = op == Type::Star
                ? a * b
                : a / b;
        }

        return left;
    }

    Value unary() {
        if (match(Type::Minus))
            return -number(unary());

        if (match(Type::Plus))
            return number(unary());

        return primary();
    }

    Value primary() {
        if (match(Type::Number)) {
            return stod(tokens[pos - 1].text);
        }

        if (match(Type::String)) {
            return tokens[pos - 1].text;
        }

        if (match(Type::LParen)) {
            Value result = expression();
            expect(Type::RParen, "Esperado ')'");
            return result;
        }

        if (match(Type::Name)) {
            string name = tokens[pos - 1].text;

            if (name == "True") return 1.0;
            if (name == "False") return 0.0;

            auto it = variables.find(name);

            if (it == variables.end())
                throw runtime_error(
                    "Variavel nao definida: " + name
                );

            return it->second;
        }

        throw runtime_error("Expressao invalida");
    }

    void statement() {
        if (current().type != Type::Name)
            throw runtime_error("Instrucao invalida");

        string name = advance().text;

        if (name == "print") {
            expect(Type::LParen, "Esperado '(' depois de print");

            bool first = true;

            while (current().type != Type::RParen) {
                if (!first)
                    expect(Type::Comma, "Esperada virgula");

                cout << (first ? "" : " ")
                     << stringify(expression());

                first = false;

                if (current().type != Type::Comma)
                    break;
            }

            expect(Type::RParen, "Esperado ')'");

            cout << '\n';
            return;
        }

        expect(Type::Equal, "Esperado '=' na atribuicao");

        variables[name] = expression();
    }

public:
    explicit Interpreter(vector<Token> code)
        : tokens(move(code)) {}

    void run() {
        while (current().type != Type::End) {
            if (match(Type::Newline))
                continue;

            try {
                statement();

                if (current().type != Type::Newline &&
                    current().type != Type::End)
                    throw runtime_error(
                        "Texto inesperado depois da instrucao"
                    );

            } catch (const exception& e) {
                cerr << "cppys: " << e.what() << '\n';
                throw;
            }
        }
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        cerr << "Uso: cppys arquivo.pys\n";
        return 1;
    }

    string filename = argv[1];

    if (filename.size() < 4 ||
        filename.substr(filename.size() - 4) != ".pys") {
        cerr << "cppys: o arquivo deve terminar em .pys\n";
        return 1;
    }

    ifstream file(filename);

    if (!file) {
        cerr << "cppys: nao foi possivel abrir o arquivo\n";
        return 1;
    }

    stringstream buffer;
    buffer << file.rdbuf();

    try {
        Lexer lexer(buffer.str());
        Interpreter interpreter(lexer.scan());
        interpreter.run();
    } catch (const exception&) {
        return 1;
    }

    return 0;
}

