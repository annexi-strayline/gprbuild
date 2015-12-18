from gprconfig_utils import *
from gnatpython.ex import Run

## Test that a language can have spaces in its name (H320-012)
create_fake_ada_compiler ("install", "i686-pc-linux-gnu", "6.1.0w", "3.4.6")

os.environ["PATH"] = os.path.sep + 'bin' + os.pathsep \
    + os.path.join(TEST_DIR, 'install', 'bin')

test_gprconfig_interactive ("""1
s""",
                            ['--target=i686-pc-linux-gnu',
                             '--config=Project File'])

diff_gpr ('expected.gpr', 'output.gpr')