import os
import shutil
import tempfile
import time
import pytest
import numpy as np


@pytest.mark.parametrize('row', range(51))
@pytest.mark.parametrize('col', range(105))
@pytest.mark.parametrize('use_irrigation', [False, True])
def test_europe(row, col, use_irrigation):
    print('=========', row, col, use_irrigation, '=========')
    cwd = os.getcwd()

    # AquaCrop executable
    executable = os.environ['AQUACROP_COMMAND']

    # Location of the INPUT folder
    test_dir = os.path.join(os.environ['AQUACROP_TEST_ROOT'], 'test_europe')
    input_dir = os.path.join(test_dir, 'INPUT')

    pixel_name = str(row) + '_' + str(col)

    # Check that a soil file exists
    soil_dir = os.path.join(input_dir, 'soil_files')
    soil_name = str(row) + '_' + str(col) + '.SOL'
    soil_file = os.path.join(soil_dir, soil_name)
    if not os.path.exists(soil_file):
        print('SKIP: missing soil file for {0}: {1}'.format(pixel_name,
                                                            soil_file))
        return

    # Create a temporary working directory for the current test
    work_dir = tempfile.mkdtemp(dir=cwd)

    irrigation_suffix = 'Irr' if use_irrigation else 'noIrr'

    output_dir = os.path.join(work_dir, 'OUTPUT_' + irrigation_suffix)
    if not os.path.exists(output_dir):
        os.mkdir(output_dir)

    # Make new local directory for each pixel
    pixel_dir = os.path.join(output_dir, pixel_name)
    if not os.path.exists(pixel_dir):
        os.mkdir(pixel_dir)
    os.chdir(pixel_dir)

    # Place Aquacrop directories SIMUL and LIST in local directory
    d = os.path.join(pixel_dir, 'SIMUL')
    if os.path.exists(d):
        shutil.rmtree(d)
    shutil.copytree(os.path.join(input_dir, 'SIMUL'), d)

    for d in [os.path.join(pixel_dir, 'OUTP'), os.path.join(pixel_dir, 'LIST')]:
        if not os.path.exists(d):
            os.mkdir(d)

    # Copy the PRM file to the temporary LIST directory
    prm_name = '{0}.PRM'.format(pixel_name)
    prm_file_inp = os.path.join(input_dir, 'prm_templates', irrigation_suffix,
                                prm_name)
    prm_file_out = os.path.join(pixel_dir, 'LIST', prm_name)
    shutil.copyfile(prm_file_inp, prm_file_out)

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
    cmd = 'sed -i "s@__OUTPUTDIR__@{0}@g" {1}'.format(output_dir, prm_file_out)
    os.system(cmd)

    listprojects_file = os.path.join(pixel_dir, 'LIST', 'ListProjects.txt')
    with open(listprojects_file, 'w') as f:
        f.write(prm_name + '\n')

    # Run Aquacrop
    cmd = os.path.join(executable)
    t_start = time.time()
    exitcode = os.system(cmd)
    t_stop = time.time()
    print('Runtime: {0:.1f} seconds'.format(t_stop - t_start))
    assert exitcode == 0, 'AquaCrop exited with exit code {0}'.format(exitcode)

    # Remove extra files and directories: only save PRM and output file
    shutil.rmtree(os.path.join(pixel_dir, 'SIMUL'))
    shutil.rmtree(os.path.join(pixel_dir, 'LIST'))
    os.remove(os.path.join(pixel_dir, 'OUTP', 'AllDone.OUT'))
    os.remove(os.path.join(pixel_dir, 'OUTP', 'ListProjectsLoaded.OUT'))

    # Check that the output is the same as the reference
    for suffix in ['day', 'season']:
        filename = '{0}PRM{1}.OUT'.format(pixel_name, suffix)

        reference_file = os.path.join(test_dir, 'OUTPUT_' + irrigation_suffix,
                                      pixel_name, 'OUTP', filename)
        with open(reference_file, 'r', encoding='ISO-8859-1') as f:
            ref_lines = f.readlines()

        output_file = os.path.join(output_dir, pixel_name, 'OUTP', filename)
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
                try:
                    assert ref_line == out_line, (i, ref_line, out_line)
                except AssertionError:
                    print('WARN: need item-by-item check (line {0})'.format(i))
                    items_ref = ref_line.split()
                    items_out = out_line.split()
                    for item_ref, item_out in zip(items_ref, items_out):
                        if item_ref != item_out:
                            assert np.isclose(float(item_ref), float(item_out))

        print('{0} checks = OK'.format(filename))

    os.chdir(cwd)
    shutil.rmtree(work_dir)
