# example-generation-experiment

### Data

The `data-raw` folder contains files downloaded from Moodle, with a set for each study including:

  * `*-tasks.csv` - with details of the Moodle question ID and our name for each question in the study,
  * `*-groups.csv` - has the anonymous identifier of each student in the course, along with the experimental group they were assigned to,
  * `*-grades.csv` - various files with the quiz results from Moodle,
  * `attempts-*.csv` - each file has all the attempts at a particular question, obtained by using the Moodle custom SQL plugin to run the following query:
  
> SELECT qa.*, qas_last.*, u.firstname, u.lastname
> FROM {question_attempts} qa
> LEFT JOIN {question_attempt_steps} qas_last ON qas_last.questionattemptid = qa.id
> /* attach another copy of qas to those rows with the most recent timecreated, using method from https://stackoverflow.com/a/28090544 */
> LEFT JOIN {question_attempt_steps} qas_prev
> 	ON qas_last.questionattemptid = qas_prev.questionattemptid
> 		AND (qas_last.timecreated < qas_prev.timecreated
> 				OR (qas_last.timecreated = qas_prev.timecreated
> 						AND qas_last.id < qas_prev.id))
> LEFT JOIN {user} u ON qas_last.userid = u.id
> WHERE
> qas_prev.timecreated IS NULL
> AND qa.`questionid` = :question_id

Since this raw data includes students who did not consent to participate in the study, it is not included in the respository. Instead, the scripts in the next section are used to filter the raw data into the `data-clean` folder, which is included in the repository.

### 1. Process attempts

There are two R scripts, `1_process_attempts_study1.R` and `1_process_attempts_study2.R` that do some minimal processing of the raw attempts data.

The raw `attempts-*.csv` files include attempts from all students, including those who did not consent to participate in the study. So these R scripts use the info from `*-groups.csv` to filter the attempts data so that only consenting students are included.

### 2. Analysis

The notebooks `study1_analysis.Rmd` and `study2_analysis.Rmd` do the main experimental analyses.

These generate the files `study1_ft.csv` and `study2_ft.csv` in the `data-clean` folder, containing the data used for the main analysis in each case (where each row is a student, and columns record the experimental group and the score on the outcome measure).


TODO - detailed analysis of the attempts (e.g. range of examples given, success on the classification task, etc)