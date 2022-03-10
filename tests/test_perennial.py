import os
import shutil
import tempfile
import time
import pytest
import numpy as np


def is_within_one_significant_digit(str_ref, str_val):
    """
    Returns whether two floating point numbers (given as strings)
    have at most a unit difference in the least significant digit
    (see the test_digits_check() test).
    """
    assert len(str_ref) == len(str_ref.strip()), str_ref
    assert len(str_val) == len(str_val.strip()), str_val

    if '.' in str_ref:
        assert '.' in str_val, (str_ref, str_val)
        # Check that both have same number of digits after the decimal
        num_ref = len(str_ref) - str_ref.index('.') - 1
        num_val = len(str_val) - str_val.index('.') - 1
        assert num_ref == num_val, (str_ref, str_val)
    else:
        assert '.' not in str_val, (str_ref, str_val)

    int_ref = int(str_ref.replace('.', '', 1))
    int_val = int(str_val.replace('.', '', 1))
    is_ok = abs(int_ref - int_val) < 2
    return is_ok



def test_perennial():
    print('========= Perennial Test =========')
    cwd = os.getcwd()

    # AquaCrop executable
    executable = os.environ['AQUACROP_COMMAND']

    # Location of the INPUT folder
    test_dir = os.path.join(os.environ['AQUACROP_TEST_ROOT'], 'test_perennial')
    input_dir = os.path.join(test_dir, 'INPUT')

    # Create a temporary working directory for the current test
    work_dir = tempfile.mkdtemp(dir=cwd)
    os.chdir(work_dir)

    # Create and populate the LIST, PARAM, OUTP
    for dirname in ['LIST', 'PARAM', 'OUTP']:
        dirpath = os.path.join(work_dir, dirname)
        os.mkdir(dirpath)

    prm_name = 'test.PRM'
    prm_file_inp = os.path.join(input_dir, prm_name)
    prm_file_out = os.path.join(work_dir, 'LIST', prm_name)
    shutil.copyfile(prm_file_inp, prm_file_out)

    ppn_name = 'test.PPn'
    ppn_file_inp = os.path.join(input_dir, ppn_name)
    ppn_file_out = os.path.join(work_dir, 'PARAM', ppn_name)
    shutil.copyfile(ppn_file_inp, ppn_file_out)

    listprojects_file = os.path.join(work_dir, 'LIST', 'ListProjects.txt')
    with open(listprojects_file, 'w') as f:
        f.write(prm_name + '\n')

    # Copy the SIMUL input directory as well
    shutil.copytree(os.path.join(input_dir, 'SIMUL'),
                    os.path.join(work_dir, 'SIMUL'))

    # Enclose all directory paths of the PRM file in quotes
    # For this to work properly, we first need to replace any DOS-style
    # line endings to UNIX-style line endings
    cmd = 'sed -i "s/\r//" {0}'.format(prm_file_out)
    os.system(cmd)
    cmd = 'sed -i -e \'s@__INPUTDIR__.*@\"&\"@\' {0}'.format(prm_file_out)
    os.system(cmd)
    cmd = 'sed -i -e \'s@__OUTPUTDIR__.*@\"&\"@\' {0}'.format(prm_file_out)
    os.system(cmd)
    # Now fill in the template paths
    cmd = 'sed -i "s@__INPUTDIR__@{0}@g" {1}'.format(input_dir, prm_file_out)
    os.system(cmd)
    cmd = 'sed -i "s@__OUTPUTDIR__@{0}@g" {1}'.format(work_dir, prm_file_out)
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
    os.remove(os.path.join(work_dir, 'OUTP', 'AllDone.OUT'))
    os.remove(os.path.join(work_dir, 'OUTP', 'ListProjectsLoaded.OUT'))

    # Check that the output is the same as the reference
    for suffix in ['day', 'season']:
        filename = 'testPRM{0}.OUT'.format(suffix)

        reference_file = os.path.join(test_dir, 'OUTP', filename)
        with open(reference_file, 'r', encoding='ISO-8859-1') as f:
            ref_lines = f.readlines()

        output_file = os.path.join(work_dir, 'OUTP', filename)
        with open(output_file, 'r', encoding='ISO-8859-1') as f:
            out_lines = f.readlines()

        assert len(ref_lines) == len(out_lines), \
               ('Different number of lines in output and reference files ',
                reference_file, output_file)

        num_col = {'day': 114, 'season': 41}[suffix]
        num_not_realclose = [0] * num_col
        num_items_total = [0] * num_col

        for i, (ref_line, out_line) in enumerate(zip(ref_lines, out_lines)):
            if i == 0:
                # Only elementary checks for the header line
                assert ref_line.startswith('AquaCrop'), (i, ref_line, out_line)
                assert out_line.startswith('AquaCrop'), (i, ref_line, out_line)
                assert 'Output created on' in out_line, (i, ref_line, out_line)
                assert 'Output created on' in ref_line, (i, ref_line, out_line)
            else:
                ref_line = ref_line.replace('°C', 'degC')

                items_ref = ref_line.split()
                for i in range(len(items_ref)):
                    num_items_total[i] += 1

                try:
                    assert ref_line == out_line, (i, ref_line, out_line)
                except AssertionError:
                    print('WARN: need item-by-item check (line {0})'.format(i))
                    items_ref = ref_line.split()
                    items_out = out_line.split()
                    for icol, (item_ref, item_out) in enumerate(zip(items_ref, 
                                                                    items_out)):
                        if item_ref != item_out:
                            is_realclose = np.isclose(float(item_ref), float(item_out))

                            if not is_realclose:
                                num_not_realclose[icol] += 1

                                try:
                                    is_close = is_within_one_significant_digit(
                                                            item_ref, item_out)

                                except AssertionError:
                                    assert False, ('failure checking digits',
                                           icol, item_ref, item_out)
                                else:
                                    assert is_close, (icol, item_ref, item_out)

        print('{0} num_items_total = {1}'.format(filename,
                                                 sum(num_items_total)))
        print('{0} num_not_realclose = {1}'.format(filename,
                                                   sum(num_not_realclose)))
        numdevlist = [i * 100. / j for (i, j) in zip(num_not_realclose,
                                                     num_items_total)]
        print('{0} numdevlist [%] = {1}'.format(filename,
                        ' '.join(map(lambda x: '%.3f' % x, numdevlist))))
        maxdev = np.max(numdevlist)
        maxcol = np.argmax(numdevlist)
        print('{0} maxnumdev [%] = {1:.3f} (column index {2})'.format(
                                            filename, maxdev, maxcol))

        # Certain small deviations are tolerated in the daily output
        tolerated_deviations = {'day': 35, 'season': 0}[suffix]
        msg = '{0} small deviation(s) currently allowed for this test case'
        print(msg.format(tolerated_deviations))

        assert int(np.sum(num_not_realclose)) <= tolerated_deviations, \
                   'more than allowed number of small deviation detected'

        print('{0} checks = OK'.format(filename))


    # Now check the DEFAULT.CRO and DEFAULT.SOL files
    for filename in ['DEFAULT.CRO', 'DEFAULT.SOL']:
        reference_file = os.path.join(input_dir, 'SIMUL', filename)
        with open(reference_file, 'r', encoding='ISO-8859-1') as f:
            ref_lines = f.readlines()

        output_file = os.path.join(work_dir, 'SIMUL', filename)
        with open(output_file, 'r', encoding='ISO-8859-1') as f:
            out_lines = f.readlines()

        assert len(ref_lines) == len(out_lines), \
               ('Different number of lines in output and reference files ',
                reference_file, output_file)

        for i, (ref_line, out_line) in enumerate(zip(ref_lines, out_lines)):
            ref_line = ref_line.replace('°C', 'degC')
            assert ref_line == out_line, (i, ref_line, out_line)

        print('{0} checks = OK'.format(filename))

    os.chdir(cwd)
    shutil.rmtree(work_dir)
