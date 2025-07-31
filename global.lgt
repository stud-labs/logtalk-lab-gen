:- object(global).

	:- public(init/0).
	:- mode(init, one).
	:- info(init/0, [
		comment is 'Read configuration file and set up static options.'
	]).

	init :-
		::at_base('syllabus_config.yaml', PathName),
		open(PathName, read, In),
		yaml_read(In, YAML),
		close(In),
		retractall(yaml_dom(_)),
		assertz(yaml_dom(YAML)),!.

	:- protected(base_config/1).
 	:- mode(base_config(-atom), zero_or_one).
 	:- info(base_config/1, [
 		comment is 'Return base directory, where config file is located',
		argnames is ['BaseDirectory']
 	]).

	base_config(base_dir('/home/eugeneai/projects/text/docent/isu/2025/cirricullum')).
%		sql_connection::connect("/home/eugeneai/projects/text/docent/isu/2025/cirricullum-2025/pmi", _).
%		sql_connection::connect("/home/eugeneai/projects/text/docent/isu/2025/cirricullum/pmi", _).
%		sql_connection::connect("/home/eugeneai/projects/text/docent/isu/2025/dev/cirricullum-2025/pmi", _).

	:- protected(yaml_dom/1).
	:- mode(yaml_dom(-atom), zero_or_one).
	:- info(yaml_dom/1, [
		comment is 'Store and return YAML DOM tree',
		argnames is ['YamlDOM']
	]).

	:- use_module(library(yaml), [yaml_read/2, yaml_write/2, yaml_write/3]).

	:- dynamic(yaml_dom/1).

	:- public(syllabus/1).
	:- mode(syllabus(?atom), zero_or_one).
	:- info(syllabus/1, [
		comment is 'Returns data about syllabus file name, location, etc.',
		argnames is ['QueryAtom']
	]).

	syllabus(path_name(PathName)) :-
		::yaml(config/dircode, DirCode),
		::yaml(config/year, Year),
		::

	:- public(yaml/3).
	:- mode(yaml(+atom, -atom, +atom), zero_or_more).
	:- info(yaml/3, [
		comment is 'Query config with a path expr, return DefaultValue if key does not exist',
		argnames is ['DictQueryPath', 'Value', 'DefaultValue']
	]).

	yaml(PathExpr, Value, Default) :-
		::yaml_dom(DOM),
		Value = DOM.get(PathExpr, Default).

	:- public(yam2/2).
	:- mode(yaml(+atom, -atom), zero_or_more).
	:- info(yaml/2, [
		comment is 'Query config with a path expression, fail, if no record',
		argnames is ['DictQueryPath', 'Value']
	]).

	yaml(PathExpr, Value) :-
		::yaml_dom(DOM),
		Value = DOM.get(PathExpr).

	:- protected(at_base/2).
	:- mode(at_base(+atom, ?atom), one).
	:- info(at_base/2, [
		comment is 'Calculate file path relatively the base path',
		argnames is ['Name', 'FilePathName']
	]).

	at_base(Name, FileName) :-
		::base_config(base_dir(D)),
		::at_dir(D, Name, FileName).

	:- protected(at_dir/3).
	:- mode(at_dir(+atom, +atom, ?atom), one).
	:- info(at_dir/3, [
		comment is 'Calculate file path relatively the dir path',
		argnames is ['BaseDirectory', 'Name', 'FilePathName']
	]).

	at_dir(Dir, Name, FileName) :-
		format(atom(FileName), '~w/~w', [Dir, Name]).

	:- initialization(::init).

:- end_object.
