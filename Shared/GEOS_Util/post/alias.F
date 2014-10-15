      function match (name,alias)
      character*256   name,alias
      logical         match
                      match = .false.

      if(     trim(name)  == 'u'       ) then
          if( trim(alias) == 'u'       ) match = .true.
          if( trim(alias) == 'U'       ) match = .true.
          if( trim(alias) == 'uwnd'    ) match = .true.
          if( trim(alias) == 'UWND'    ) match = .true.
          if( trim(alias) == 'ugrdprs' ) match = .true.
          if( trim(alias) == 'UGRDPRS' ) match = .true.
          if( trim(alias) == 'ugrd'    ) match = .true.
          if( trim(alias) == 'UGRD'    ) match = .true.
      endif

      if(     trim(name)  == 'v'       ) then
          if( trim(alias) == 'v'       ) match = .true.
          if( trim(alias) == 'V'       ) match = .true.
          if( trim(alias) == 'vwnd'    ) match = .true.
          if( trim(alias) == 'VWND'    ) match = .true.
          if( trim(alias) == 'vgrdprs' ) match = .true.
          if( trim(alias) == 'VGRDPRS' ) match = .true.
          if( trim(alias) == 'vgrd'    ) match = .true.
          if( trim(alias) == 'VGRD'    ) match = .true.
      endif

      if(     trim(name)  == 't'       ) then
          if( trim(alias) == 't'       ) match = .true.
          if( trim(alias) == 'T'       ) match = .true.
          if( trim(alias) == 'tmpu'    ) match = .true.
          if( trim(alias) == 'TMPU'    ) match = .true.
          if( trim(alias) == 'tmpprs'  ) match = .true.
          if( trim(alias) == 'TMPPRS'  ) match = .true.
          if( trim(alias) == 'tmp'     ) match = .true.
          if( trim(alias) == 'TMP'     ) match = .true.
      endif

      if(     trim(name)  == 'q'       ) then
          if( trim(alias) == 'q'       ) match = .true.
          if( trim(alias) == 'Q'       ) match = .true.
          if( trim(alias) == 'qv'      ) match = .true.
          if( trim(alias) == 'QV'      ) match = .true.
          if( trim(alias) == 'sphu'    ) match = .true.
          if( trim(alias) == 'SPHU'    ) match = .true.
      endif

      if(     trim(name)  == 'h'       ) then
          if( trim(alias) == 'h'       ) match = .true.
          if( trim(alias) == 'H'       ) match = .true.
          if( trim(alias) == 'hght'    ) match = .true.
          if( trim(alias) == 'HGHT'    ) match = .true.
          if( trim(alias) == 'hgtprs'  ) match = .true.
          if( trim(alias) == 'HGTPRS'  ) match = .true.
          if( trim(alias) == 'hgt'     ) match = .true.
          if( trim(alias) == 'HGT'     ) match = .true.
      endif

      if(     trim(name)  == 'th'      ) then
          if( trim(alias) == 'th'      ) match = .true.
          if( trim(alias) == 'TH'      ) match = .true.
          if( trim(alias) == 'theta'   ) match = .true.
          if( trim(alias) == 'THETA'   ) match = .true.
      endif

      if(     trim(name)  == 'tv'      ) then
          if( trim(alias) == 'tv'      ) match = .true.
          if( trim(alias) == 'TV'      ) match = .true.
      endif

      if(     trim(name)  == 'thv'     ) then
          if( trim(alias) == 'thv'     ) match = .true.
          if( trim(alias) == 'THV'     ) match = .true.
          if( trim(alias) == 'thetav'  ) match = .true.
          if( trim(alias) == 'THETAV'  ) match = .true.
      endif

      if(     trim(name)  == 'ps'      ) then
          if( trim(alias) == 'ps'      ) match = .true.
          if( trim(alias) == 'PS'      ) match = .true.
      endif

      if(     trim(name)  == 'dp'      ) then
          if( trim(alias) == 'dp'      ) match = .true.
          if( trim(alias) == 'DP'      ) match = .true.
          if( trim(alias) == 'delp'    ) match = .true.
          if( trim(alias) == 'DELP'    ) match = .true.
      endif

      if(     trim(name)  == 'pl'      ) then
          if( trim(alias) == 'pl'      ) match = .true.
          if( trim(alias) == 'PL'      ) match = .true.
      endif

      if(     trim(name)  == 'ple'     ) then
          if( trim(alias) == 'ple'     ) match = .true.
          if( trim(alias) == 'PLE'     ) match = .true.
      endif

      if(     trim(name)  == 'phis'    ) then
          if( trim(alias) == 'phis'    ) match = .true.
          if( trim(alias) == 'PHIS'    ) match = .true.
          if( trim(alias) == 'gz'      ) match = .true.
          if( trim(alias) == 'GZ'      ) match = .true.
      endif

      if(     trim(name)  == 'slp'     ) then
          if( trim(alias) == 'slp'     ) match = .true.
	  if( trim(alias) == 'SLP'     ) match = .true.
          if( trim(alias) == 'SLPRS'   ) match = .true.
          if( trim(alias) == 'prmslmsl') match = .true.
          if( trim(alias) == 'PRMSLMSL') match = .true.
          if( trim(alias) == 'prmsl'   ) match = .true.
          if( trim(alias) == 'PRMSL'   ) match = .true.
      endif

      return
      end
