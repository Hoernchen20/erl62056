-type device_address() :: binary().
-type request_message() :: {request_message, device_address()}.

-type manufacturer_id() :: binary().
-type identification() :: binary().
-type identification_message() ::
    {identification_message, manufacturer_id(), baudrate(), identification()}.

-type protocol_mode() :: normal.
-type baudrate() :: 300 | 600 | 1200 | 2400 | 4800 | 9600 | 19200.
-type control_mode() :: data_readout | programming_mode.
-type ack_option_select_message() ::
    {ack_option_select_message, protocol_mode(), baudrate(), control_mode()}.

-type data_set() :: {binary(), binary(), binary() | undefined}.
-type data_message() :: {data_message, [data_set()]}.
