import sublime, sublime_plugin, os, re
import subprocess

def is_haskell_file(filename):
    # return False
    return filename[-3:] == '.hs'

def remove_last_word(s):
    return ' '.join(s.split()[:-1])

def check_output(command):
    return subprocess.Popen(command, stdout=subprocess.PIPE).communicate()[0]

class Completer(sublime_plugin.EventListener):
    def on_query_completions(self, view, prefix, locations):
        if is_haskell_file(view.file_name()):
            region = view.sel()[0]
            line = view.substr(view.line(region))
            expr = '(' + remove_last_word(line.split('=', 1)[1]).strip() + ')'
            suggestions = check_output(['/Users/izzy/bin/HasKomplete', expr]).splitlines()

            # sugs = []
            # for s in suggestions:
            #     name, typ = s.split('::')
            #     sugs.append(('::' + typ, s.strip()))

            # print sugs

            return [(s, '') for s in suggestions]
