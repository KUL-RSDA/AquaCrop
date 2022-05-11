"""
Test for checking the contents of the optional
"evaluation" and "harvest" output files.
"""
import os
import shutil
import tempfile
import time
import pytest
import numpy as np


def test_harvest():
    print('========= Harvest Test =========')
    cwd = os.getcwd()

    # AquaCrop executable
    executable = os.environ['AQUACROP_COMMAND']

    # Location of the INPUT folder
    test_dir = os.path.join(os.environ['AQUACROP_TEST_ROOT'], 'test_harvest')
    input_dir = os.path.join(test_dir, 'INPUT')

    # Create a temporary working directory for the current test
    work_dir = tempfile.mkdtemp(dir=cwd)
    os.chdir(work_dir)

    # Create and populate the LIST, PARAM, OUTP
    for dirname in ['LIST', 'PARAM', 'OUTP']:
        dirpath = os.path.join(work_dir, dirname)
        os.mkdir(dirpath)

    pro_name = 'test_default.PRO'
    pro_file_inp = os.path.join(input_dir, pro_name)
    pro_file_out = os.path.join(work_dir, 'LIST', pro_name)
    shutil.copyfile(pro_file_inp, pro_file_out)

    pp1_name = 'test_default.PP1'
    pp1_file_inp = os.path.join(input_dir, pp1_name)
    pp1_file_out = os.path.join(work_dir, 'PARAM', pp1_name)
    shutil.copyfile(pp1_file_inp, pp1_file_out)

    listprojects_file = os.path.join(work_dir, 'LIST', 'ListProjects.txt')
    with open(listprojects_file, 'w') as f:
        f.write(pro_name + '\n')

    # Copy the SIMUL input directory as well
    shutil.copytree(os.path.join(input_dir, 'SIMUL'),
                    os.path.join(work_dir, 'SIMUL'))

    # Enclose all directory paths of the PRM file in quotes
    # For this to work properly, we first need to replace any DOS-style
    # line endings to UNIX-style line endings
    cmd = 'sed -i "s/\r//" {0}'.format(pro_file_out)
    os.system(cmd)
    cmd = 'sed -i -e \'s@__INPUTDIR__.*@\"&\"@\' {0}'.format(pro_file_out)
    os.system(cmd)
    cmd = 'sed -i -e \'s@__OUTPUTDIR__.*@\"&\"@\' {0}'.format(pro_file_out)
    os.system(cmd)
    # Now fill in the template paths
    cmd = 'sed -i "s@__INPUTDIR__@{0}@g" {1}'.format(input_dir, pro_file_out)
    os.system(cmd)
    cmd = 'sed -i "s@__OUTPUTDIR__@{0}@g" {1}'.format(work_dir, pro_file_out)
    os.system(cmd)

    # Run Aquacrop
    cmd = os.path.join(executable)
    t_start = time.time()
    exitcode = os.system(cmd)
    t_stop = time.time()
    print('Runtime: {0:.1f} seconds'.format(t_stop - t_start))
    assert exitcode == 0, 'AquaCrop exited with exit code {0}'.format(exitcode)

    # Remove extra files and directories: only save PRM and output file
    shutil.rmtree(os.path.join(work_dir, 'PARAM'))
    shutil.rmtree(os.path.join(work_dir, 'LIST'))
    shutil.rmtree(os.path.join(work_dir, 'SIMUL'))
    os.remove(os.path.join(work_dir, 'OUTP', 'AllDone.OUT'))
    os.remove(os.path.join(work_dir, 'OUTP', 'ListProjectsLoaded.OUT'))

    # Check that the output is the same as the reference
    for suffix in ['day', 'season', 'harvests', 'evaluation']:
        filename = 'test_defaultPRO{0}.OUT'.format(suffix)

        reference_file = os.path.join(test_dir, 'OUTP', filename)
        if filename.endswith('day.OUT') or filename.endswith('season.OUT'):
            encoding = 'ISO-8859-1'
        else:
            encoding = 'utf-8'
        with open(reference_file, 'r', encoding=encoding) as f:
            ref_lines = f.readlines()

        output_file = os.path.join(work_dir, 'OUTP', filename)
        with open(output_file, 'r', encoding='ISO-8859-1') as f:
            out_lines = f.readlines()

        assert len(ref_lines) == len(out_lines), \
               ('Different number of lines in output and reference files ',
                reference_file, output_file)

        for i, (ref_line, out_line) in enumerate(zip(ref_lines, out_lines)):
            if i == 0:
                # Only elementary checks for the header line
                assert ref_line.startswith('AquaCrop'), (i, ref_line, out_line)
                assert out_line.startswith('AquaCrop'), (i, ref_line, out_line)
                assert 'Output created on' in out_line, (i, ref_line, out_line)
                assert 'Output created on' in ref_line, (i, ref_line, out_line)
            else:
                ref_line = ref_line.replace('  °C', 'degC')
                assert ref_line == out_line, (i, ref_line, out_line)

        print('{0} checks = OK'.format(filename))

    os.chdir(cwd)
    shutil.rmtree(work_dir)
