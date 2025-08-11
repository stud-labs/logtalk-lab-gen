
:- set_prolog_flag(stack_limit, 8_147_483_648).
:- set_logtalk_flag(unknown_entities, silent).

:- initialization((
    % set project-specific global flags
    set_logtalk_flag(report, warnings),
    set_logtalk_flag(events, allow),
    set_prolog_flag(verbose_load, true),
    logtalk_load(tutor(loader)),
    logtalk_load(tools(loader)),  % debugging, tracing, trace
    logtalk_load(debugger(loader)),  % debugging
    logtalk_load([
		morpher
	 , global]),
	 logtalk_load([
	   docs
	 ], [debug(off)]),
    % logtalk_load(options(loader)),
    % load the project source files
    % set_logtalk_flag(debug, on),
    logtalk_load([
      parts
    ], [debug(on)]),
    % set_logtalk_flag(debug, off),
    logtalk_load([
	   cd_source
	 , cd_docs
    ], [debug(on)]),
		Code = 'Б1.В.ДВ.01.01',
		forall(
			(
				Disc = cd_discipline(Code, Title),
				Disc::title(Code, Title)
			),
			(
				Doc = cd_documents(Disc),
				Doc::discipline(Disc),
				Doc::gen,
				format(
					'INFO: We generated documents for ~q discipline!~n',
					[Title])
			)
		),
    halt
)).
