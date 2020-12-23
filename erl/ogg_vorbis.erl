-module(ogg_vorbis).
-export([granulepos/6]).

granulepos(_, PrevPos, PrevCount, _, [], <<>>) ->
	{PrevPos, PrevCount, <<>>};
granulepos(IsPartial, PrevPos, none, ModeInfo, [Len|Pktlens], Body) ->
	<<Pkt:Len/bytes, Rest/bytes>> = Body,
	SampleCount = vorbis_headers:audio_sample_count(ModeInfo, Pkt) div 2,
	granulepos(IsPartial, PrevPos+SampleCount, SampleCount, ModeInfo, Pktlens, Rest);
granulepos(true, PrevPos, PrevCount, ModeInfo, [Len, PartialLen], Body) ->
	<<Pkt:Len/bytes, PartialPkt:PartialLen/bytes>> = Body,
	SampleCount = vorbis_headers:audio_sample_count(ModeInfo, Pkt),
	{PrevPos+((PrevCount + SampleCount) div 4), SampleCount, PartialPkt};
granulepos(false, PrevPos, PrevCount, ModeInfo, [Len], Body) ->
	<<Pkt:Len/bytes>> = Body,
	SampleCount = vorbis_headers:audio_sample_count(ModeInfo, Pkt),
	{PrevPos+((PrevCount + SampleCount) div 4), SampleCount, <<>>};
granulepos(IsPartial, PrevPos, PrevCount, ModeInfo, [Len|Pktlens], Body) ->
	<<Pkt:Len/bytes, Rest/bytes>> = Body,
	SampleCount = vorbis_headers:audio_sample_count(ModeInfo, Pkt),
	granulepos(IsPartial, PrevPos+((PrevCount + SampleCount) div 4), SampleCount, ModeInfo, Pktlens, Rest).
