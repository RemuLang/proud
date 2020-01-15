from proud.core_lang import sexpr
from yapf.yapflib.yapf_api import FormatCode


def generate(self: sexpr.ASTTag):
    args = []
    anns = []
    for e in self.args:
        if isinstance(e, tuple):
            args.append(e[0])
            anns.append(e[0] + ': ' + e[1])
        else:
            args.append(e)
            anns.append(e)
    scopes = {self.name, *args}
    modname = 'module'
    while modname in scopes:
        modname += '_eval'

    cls = 'class Eval_{0}(Protocol):\n' \
          '    @abc.abstractmethod\n'\
          '    def {0}({1}, {2}): ...\n'.format(
        self.name, modname, ', '.join(anns))

    return cls + 'def {}({}):\n    return {}'.format(
        self.name, ', '.join(anns), 'lambda {0}: {0}.{1}({2})'.format(
            modname, self.name, ', '.join(args)))


code = [
    'import builtins', "import abc",
    'from typing import Tuple', 'from proud.sexpr import *',
    'from typing_extensions import Protocol', 'dispatcher = {}'
]
for k, v in sexpr.__dict__.items():
    if isinstance(v, sexpr.ASTTag):
        code.append(generate(v))
        code.append(f'dispatcher[{k}] = {v.name}')


with open('proud/core_lang/composable_evaluator.py', 'w') as f:
    f.write(FormatCode('\n'.join(code))[0])
