IF OBJECT_ID('@work_database_schema.cohort_inclusion', 'U') IS NOT NULL
  DROP TABLE @work_database_schema.cohort_inclusion;

IF OBJECT_ID('@work_database_schema.cohort_inclusion_result', 'U') IS NOT NULL
  DROP TABLE @work_database_schema.cohort_inclusion_result;

IF OBJECT_ID('@work_database_schema.cohort_inclusion_stats', 'U') IS NOT NULL
  DROP TABLE @work_database_schema.cohort_inclusion_stats;

IF OBJECT_ID('@work_database_schema.cohort_summary_stats', 'U') IS NOT NULL
  DROP TABLE @work_database_schema.cohort_summary_stats;
  
CREATE TABLE @work_database_schema.cohort_inclusion (
	cohort_definition_id INT NOT NULL,
	rule_sequence INT NOT NULL,
	NAME VARCHAR(255) NULL,
	description VARCHAR(1000) NULL
	);

CREATE TABLE @work_database_schema.cohort_inclusion_result (
	cohort_definition_id INT NOT NULL,
	inclusion_rule_mask BIGINT NOT NULL,
	person_count BIGINT NOT NULL
	);

CREATE TABLE @work_database_schema.cohort_inclusion_stats (
	cohort_definition_id INT NOT NULL,
	rule_sequence INT NOT NULL,
	person_count BIGINT NOT NULL,
	gain_count BIGINT NOT NULL,
	person_total BIGINT NOT NULL
	);

CREATE TABLE @work_database_schema.cohort_summary_stats (
	cohort_definition_id INT NOT NULL,
	base_count BIGINT NOT NULL,
	final_count BIGINT NOT NULL
	);
