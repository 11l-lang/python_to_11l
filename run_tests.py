import re, tokenizer, parse, os, tempfile

def kdiff3(str1, str2):
    for envvar in ['ProgramFiles', 'ProgramFiles(x86)', 'ProgramW6432']:
        os.environ["PATH"] += os.pathsep + os.getenv(envvar, '') + r'\KDiff3'
    command = 'kdiff3'
    for file in [('left', str1), ('right', str2)]:
        full_fname = os.path.join(tempfile.gettempdir(), file[0])
        command += ' "' + full_fname + '"'
        open(full_fname, "wt", encoding='utf-8-sig').write(file[1])
    os.system(command)

for file_name in ["tests/tokenizer/errors.txt", "tests/parser/errors.txt"]:
    for test in open(file_name, encoding="utf8").read().split("\n\n\n"):
        if test.startswith('---'):
            continue
        test_source = ""
        error = None
        i = 0
        while i < len(test):
            line_start = i
            while True:
                if test[i] not in (' ', "\t"):
                    break
                i += 1
                assert(i < len(test))

            if test[i] == '^':
                message_start = i
                while True:
                    i = test.find("\n", i+1)
                    if i == -1:
                        i = len(test)
                    elif test[i-1] == '\\':
                        continue
                    break
                message = test[message_start+1:i].replace("\\\n", "\n")
                position = message_start - line_start + len(test_source) - last_line_len - 1
                if message.startswith('Error: '):
                    error = (message, position)

            else:
                i = test.find("\n", i)
                if i == -1:
                    i = len(test)
                test_source += test[line_start:i] + "\n"
                last_line_len = i - line_start

            if i < len(test):
                assert(test[i] == "\n")
                i += 1

        was_error = False
        try:
            if "tokenizer" in file_name:
                tokenizer.tokenize(test_source)
            else:
                parse.parse_and_to_str(tokenizer.tokenize(test_source), test_source, file_name)
        except (tokenizer.Error, parse.Error) as e:
            if (type(e) == tokenizer.Error) != (file_name == "tests/tokenizer/errors.txt"):
                print("Wrong type of error in file '" + file_name + "' in test:\n" + test)
                exit(1)
            was_error = True
            if error and 'Error: ' + e.message == error[0] and e.pos == error[1]:
                print('OK (Error)')
                continue
            else:
                kdiff3('Error: ' + e.message, error[0] if error is not None else '')
                next_line_pos = test_source.find("\n", e.pos)
                if next_line_pos == -1:
                    next_line_pos = len(test_source)
                prev_line_pos = test_source.rfind("\n", 0, e.pos) + 1
                # print('Error: ' + e.message + "\n" + test_source[prev_line_pos:next_line_pos] + "\n" + ' '*(e.pos - prev_line_pos) + '^')
                print('Error: ' + e.message + "\n" + test_source[prev_line_pos:next_line_pos] + "\n" + re.sub(r'[^\t]', ' ',
                                                                                                              test_source[
                                                                                                              prev_line_pos:e.pos]) + '^')
                print("in file '" + file_name + "' in test:\n" + test)
                exit(1)
        except Exception as e:
            print("Exception in file '" + file_name + "' in test:\n" + test)
            raise e
        if error is not None and not was_error:
            print("There should be error in test:\n" + test)
            exit(1)
        print('OK')

for test in open("tests/tokenizer/tokens.txt", encoding="utf8").read().split("\n\n\n"):
    source, expected_tokens = test.split("===\n")
    tokens = "\n".join([t.to_str(source) for t in tokenizer.tokenize(source)])
    if tokens != expected_tokens:
        print("Tokens mismatch for test:\n" + source + "Tokens:\n" + tokens + "\nExpected tokens:\n" + expected_tokens)
        exit(1)
    else:
        print("OK")

file_name = "tests/parser/samples.txt"
for test in open(file_name, encoding="utf8").read().split("\n\n\n"):
    if test.startswith('---'):
        continue
    source, expected_translated_source = test.split("===\n")
    expected_translated_source += "\n"
    try:
        translated_source = parse.parse_and_to_str(tokenizer.tokenize(source), source, file_name)
    except parse.Error as e:
        next_line_pos = source.find("\n", e.pos)
        if next_line_pos == -1:
            next_line_pos = len(source)
        prev_line_pos = source.rfind("\n", 0, e.pos) + 1
        print('Error: ' + e.message + ' at ' + str(e.pos) + "\n" + source[prev_line_pos:next_line_pos] + "\n" + re.sub(r'[^\t]', ' ',
                                                                                                                       source[
                                                                                                                       prev_line_pos:e.pos]) + '^')
        print("in file '" + file_name + "' in test:\n" + test)
        exit(1)
    except Exception as e:
        print("Exception in file '" + file_name + "' in test:\n" + test)
        raise e
    if translated_source != expected_translated_source:
        print("Mismatch for test:\n" + source + "Output:\n" + translated_source + "\nExpected output:\n" + expected_translated_source + "[in file '" + file_name + "']")
        kdiff3(translated_source, expected_translated_source)
        exit(1)
    else:
        print("OK")

print('All OK')
