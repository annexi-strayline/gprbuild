from gprbuild_utils import *

# This test makes sure that in the presence of aggregate projects, gprconfig
# is spawned with a list of languages including the languages from all
# aggregated projects.

gprbuild("-Pbuild_all.gpr")
run("lls_c_obj/main")