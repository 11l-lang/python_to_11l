import re, tokenizer, parse

for file_name in ["tests/tokenizer/errors.txt", "tests/parser/errors.txt"]:
    for test in open(file_name, encoding="utf8").read().split("\n\n\n"):
        if test.startswith('---'):
            continue
        test_source = ""
        error = None
        last_line_len = 0
        def process_line(line):
            global test_source, last_line_len, error
            if line.startswith('^') or line.endswith("^"):
                if line.startswith('^'):
                    position = re.search(r'[^\t ]', line[1:]).start() + 1
                    message = line[position:]
                else:
                    position = 0
                    message = line[:-1]
                position += len(test_source) - last_line_len - 1
                if message.startswith('Error: '):
                    error = (message, position)
                return
            test_source += line + "\n"
            last_line_len = len(line)

        line_start = 0
        i = 0
        while i < len(test):
            if test[i] == "\n":
                if test[i-3:i] == '```':
                    i = test.find('```', i)
                    assert(i != -1)
                    i += 3
                process_line(test[line_start:i])
                line_start = i+1
            i += 1
        process_line(test[line_start:i])

        was_error = False
        try:
            if "tokenizer" in file_name:
                tokenizer.tokenize(test_source)
            else:
                parse.parse(tokenizer.tokenize(test_source), test_source)
        except Exception as e:
            #assert(isinstance(e, tokenizer.Error) or isinstance(e, parse.Error))
            if not (isinstance(e, tokenizer.Error) or isinstance(e, parse.Error)):
                raise e
            was_error = True
            if error and "Error: " + e.message == error[0] and e.pos == error[1]:
                print('OK (Error)')
                continue
            else:
                next_line_pos = test_source.find("\n", e.pos)
                if next_line_pos == -1:
                    next_line_pos = len(test_source)
                prev_line_pos = test_source.rfind("\n", 0, e.pos) + 1
                # print('Error: ' + e.message + "\n" + test_source[prev_line_pos:next_line_pos] + "\n" + ' '*(e.pos - prev_line_pos) + '^')
                print('Error: ' + e.message + "\n" + test_source[prev_line_pos:next_line_pos] + "\n" + re.sub(r'[^\t]', ' ',
                                                                                                              test_source[
                                                                                                              prev_line_pos:e.pos]) + '^')
                print("in test:\n" + test)
                break
        if error != None and not was_error:
            print("There should be error in test:\n" + test)
            break
        print('OK')

for test in open("tests/tokenizer/tokens.txt", encoding="utf8").read().split("\n\n\n"):
    source, expected_tokens = test.split("===\n")
    tokens = "\n".join([t.to_str(source) for t in tokenizer.tokenize(source)])
    if tokens != expected_tokens:
        print("Tokens mismatch for test:\n" + source + "Tokens:\n" + tokens + "\nExpected tokens:\n" + expected_tokens)
        break
    else:
        print("OK")

for test in open("tests/parser/samples.txt", encoding="utf8").read().split("\n\n\n"):
    if test.startswith('---'):
        continue
    source, expected_translated_source = test.split("===\n")
    expected_translated_source += "\n"
    try:
        translated_source = parse.parse(tokenizer.tokenize(source), source).to_str()
    except parse.Error as e:
        next_line_pos = source.find("\n", e.pos)
        if next_line_pos == -1:
            next_line_pos = len(source)
        prev_line_pos = source.rfind("\n", 0, e.pos) + 1
        print('Error: ' + e.message + ' at ' + str(e.pos) + "\n" + source[prev_line_pos:next_line_pos] + "\n" + re.sub(r'[^\t]', ' ',
                                                                                                                       source[
                                                                                                                       prev_line_pos:e.pos]) + '^')
        print("in test:\n" + test)
        break
    if translated_source != expected_translated_source:
        print("Mismatch for test:\n" + source + "Output:\n" + translated_source + "\nExpected output:\n" + expected_translated_source)
        break
    else:
        print("OK")
