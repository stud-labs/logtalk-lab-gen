:- category(yamlc).

	:- info([
		version is 1:0:0,
		author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
		date is 2025-08-01,
		comment is 'YAML processing category'
	]).

	:- use_module(library(lists), [member/2]).

	:- public(yaml/3).
	:- mode(yaml(+atom, -atom, ?atom), one).
	:- info(yaml/3, [
		comment is 'Query config with a path expr, return DefaultValue if key does not exist',
		argnames is ['DictQueryPath', 'Value', 'DefaultValue']
	]).

	yaml(PathExpr, Value, _) :-
		::yaml_dom(DOM),
		yaml_path(DOM, PathExpr, Value), !.

	yaml(_, Default, Default).

	:- public(yaml/2).
	:- mode(yaml(+atom, -atom), zero_or_more).
	:- info(yaml/2, [
		comment is 'Query config with a path expression, fail, if no record',
		argnames is ['DictQueryPath', 'Value']
	]).

	yaml(PathExpr, Value) :-
		::yaml_dom(DOM),
		yaml_path(DOM, PathExpr, Value).

	:- public(yaml_path/3).
	:- mode(yaml_path(+atom, +atom, -atom), zero_or_more).
	:- info(yaml_path/3, [
		comment is 'Query config with a path expr to a YAML structure',
		argnames is ['YAML', 'Query', 'Value']
	]).

	yaml_path(YAML, Expr, Result) :-
		Expr = PathExpr/(Key=Value), !,
		yaml_path(YAML, PathExpr, List),
		member(Result, List),
		Value is Result.get(Key).

	yaml_path(YAML, Expr, Result) :-
		Expr = PathExpr/[Key=Value | T], !,
		yaml_path(YAML, PathExpr/(Key=Value), Result),
		check_dict(Result, T).

	yaml_path(YAML, PathExpr, Value) :-
		Value = YAML.get(PathExpr).

	check_dict(_, []) :-!.
	check_dict(Dict, [Key=Value | T]) :-
		Value = Dict.get(Key),
		check_dict(Dict, T).

	:- public(yaml_path/4).
	:- mode(yaml_path(+atom, +atom, -atom, ?atom), one).
	:- info(yaml_path/4, [
		comment is 'Query config with a path expr to a YAML structure, return DefaultValue if key does not exist',
		argnames is ['YAML', 'Query', 'Value', 'DefaultValue']
	]).

	yaml_path(YAML, PathExpr, Value, _) :-
		yaml_path(YAML, PathExpr, Value), !.

	yaml_path(_, _, Default, Default).

	:- public(yaml_load/2).
	:- mode(yaml_load(+atom, -atom), zero_or_one).
	:- info(yaml_load/2, [
		comment is 'Load YAML',
		argnames is ['FileName', 'YAML']
	]).

	:- use_module(library(yaml),
		[yaml_read/2]
	).

	yaml_load(FileName, YAML) :-
		catch(
			open(FileName, read, In),
			(
				format("ERROR: cannot open YAML file ''!", [FileName]),
				halt
			),
			true),
		yaml_read(In, YAML),
		close(In).

:- end_category.

:- object(global,
	imports(yamlc)).

	:- public(init/0).
	:- mode(init, one).
	:- info(init/0, [
		comment is 'Read configuration file and set up static options.'
	]).

	init :-
		::at_base('syllabus_config.yaml', PathName),
		^^yaml_load(PathName, YAML),
		retractall(yaml_dom(_)),
		assertz(yaml_dom(YAML)),!.

	:- protected(base_config/1).
 	:- mode(base_config(-atom), zero_or_one).
 	:- info(base_config/1, [
 		comment is 'Return base directory, where config file is located',
		argnames is ['BaseDirectory']
 	]).

	:- use_module(user, [working_directory/2, absolute_file_name/3]).

	base_config(base_dir(cwd(FileName, Dir))) :-
		working_directory(CWD, CWD),
		absolute_file_name(syllabus, Dir1, [relative_to(CWD)]),
		absolute_file_name(FileName, Dir, [relative_to(Dir1)]).

	:- protected(yaml_dom/1).
	:- dynamic(yaml_dom/1).
	:- mode(yaml_dom(-atom), zero_or_one).
	:- info(yaml_dom/1, [
		comment is 'Store and return YAML DOM tree',
		argnames is ['YamlDOM']
	]).


	:- use_module(library(yaml), [yaml_read/2, yaml_write/2, yaml_write/3]).

	:- public(syllabus/1).
	:- mode(syllabus(?atom), zero_or_one).
	:- info(syllabus/1, [
		comment is 'Returns data about syllabus file name, location, etc.',
		argnames is ['QueryAtom']
	]).

	:- use_module(library(lists), [member/2]).

	syllabus(path_name(PathName)) :-
		::direction(path_name(Direction, Path)),
		SQLIte = Direction.get(database/sqlite),
		::at_dir(Path, SQLIte, PathName).

	:- public(direction/1).
	:- mode(direction(-atom), zero_or_one).
	:- info(direction/1, [
		comment is 'Recognize direction dictionary in the YAML',
		argnames is ['YAMLDictionary']
	]).

	direction(path_name(Direction, Path)) :-
		::yaml(config/dircode, DirCode),
		::yaml(config/year, Year),
		::yaml(directions/[year=Year,code=DirCode], Direction),
		Year = Direction.get(year),
		DirCode = Direction.get(code), !,
		(
			Path = Direction.get(basepath)
			;
			RelPath = Direction.get(relpath),
			base_config(base_dir(cwd(RelPath, Path)))
		).

	:- public(crm/1).
	:- mode(crm(-atom), zero_or_one).
	:- info(crm/1, [
		comment is 'Return CRM path name, depending context',
		argnames is ['CRMConfigData']
	]).

	crm(path_name(PathName)) :-
		::direction(path_name(D, Path)),
		CRM = D.get(crm),
		::at_dir(Path, CRM, PathName).

	:- protected(at_base/2).
	:- mode(at_base(+atom, ?atom), one).
	:- info(at_base/2, [
		comment is 'Calculate file path relatively the base path',
		argnames is ['Name', 'FilePathName']
	]).

	at_base(Name, FileName) :-
		::base_config(base_dir(cwd(Name,FileName))), !.

	at_base(Name, FileName) :-
		::base_config(base_dir(D)), !,
		absolute_file_name(Name, FileName, [relative_to(D)]).

	:- protected(at_dir/3).
	:- mode(at_dir(+atom, +atom, ?atom), one).
	:- info(at_dir/3, [
		comment is 'Calculate file path relatively the dir path',
		argnames is ['BaseDirectory', 'Name', 'FilePathName']
	]).

	at_dir(Dir, Name, FileName) :-
		absolute_file_name(Name, FileName, [relative_to(Dir)]).

	:- initialization(::init).

:- end_object.
