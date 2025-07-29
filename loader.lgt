
:- set_prolog_flag(stack_limit, 8_147_483_648).
:- set_logtalk_flag(unknown_entities, silent).

:- initialization((
    % set project-specific global flags
    set_logtalk_flag(report, warnings),
    set_logtalk_flag(events, allow),
    set_logtalk_flag(debug, on),
    set_prolog_flag(verbose_load, true),
    logtalk_load(tutor(loader)),
    logtalk_load(tools(loader)),  % debugging, tracing, trace
    logtalk_load(debugger(loader)),  % debugging
    % logtalk_load(options(loader)),
    % load the project source files
    logtalk_load([
		morpher
    , docs
    , parts
	 , cd_source
%    , docs_test_simple
	 , cd_docs

    ]),
		Code = 'Б1.В.ДВ.01.01',
		Doc = cd_documents(Code, Title),
		Doc::gen,
		Disc=cd_discipline(Code, Title),
		Doc::discipline(Disc),
		Disc::title(Code, Title),
		format(
			'INFO: We generated documents for ~q discipline!~n',
	 		[Title]),
    halt
)).
