IF OBJECT_ID('@work_database_schema.@study_cohort_table', 'U') IS NOT NULL
	DROP TABLE @work_database_schema.@study_cohort_table;

CREATE TABLE @work_database_schema.@study_cohort_table (
	cohort_definition_id INT,
	subject_id BIGINT,
	cohort_start_date DATE,
	cohort_end_date DATE
	);
