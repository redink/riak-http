-define(ASSERT_EQ(A, B), if 
			     A =:= B -> ok;
			     true -> error_logger:info_report({module,?MODULE},
							      {line,?LINE},
							      {self,self()},
							      {message "Assert equal fail"})
			 end).

-define(LOG(Message), error_logger:info_report([{module, ?MODULE}, 
                              {line, ?LINE},
                              {self, self()}, 
                              {message, Message}
                             ])).
