## Social vulnerability & SEMS NPL sites study

---

#### Jacob J. Bukoski & Rosanna Neuhausler

Repository for all code and analyses assessing social vulnerability of communities in proximity to SEMS NPL sites.

The directory structure of this repository is as follows:

  1. `doc` -- directory for development of documents *
  2. `src` -- all source scripts with directory-specific readme detailing their use
  3. `bin` -- all binary scripts
  4. `raw` -- all raw datasets; downloaded directly from stable URLs *
  5. `data` -- all processed datasets *
  6. `results` -- output folder for all analyses; contains runall.sh, which performs full analysis; results are organized chronologically, using `yyyy-mm-dd` format.
  
**Note:** directory labeled with * are included in `.gitignore` and omitted from github.

The empty directories are maintained on GitHub such that cloning the repository will provide the necessary directory structure to run all code and reproduce all datasets.

---

#### Overview

The code is structured that all helper scripts and analyses are stored in the `src` and `bin` directories.

The full analysis can be run via the `runall.sh` driver script in the `results` directory. The driver script works from start to finish, checking for the existence of datasets and results and running the necessary code to generate them if they are absent (i.e., a restartable script).

To regenerate output, simply delete the relevant files and re-run `runall.sh`.

---

#### Abstraction of methods

1. Download stable datasets (`get_data.R`)
    * Data is downloaded and unzipped into `raw` directory
2. Clip raw data (`process_data.R`)
    * Clip counties to just those counties in which NPL sites exist. 
    * Clip SoVI census blocks to those intersected 5 km buffered NPL counties.
    * Input data from `raw`, output data to `data`.
3. Summarize SoVI data by site using each of the three methods
4. Drop random points within counties and summarize SoVI data by random points (additional steps here)
5. Compare NPL SoVI data vs. non-NPL points data
6. Repeat for all years of SoVI data; examine directional trends in SoVI at NPL vs. non-NPL sites

---

#### Notes:

- NPL site listed as in NC yet falls geographically in TN (site_id: 0409895)
