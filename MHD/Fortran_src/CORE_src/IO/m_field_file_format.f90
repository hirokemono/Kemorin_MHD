!m_field_file_format.f90
!      module m_field_file_format
!
!      Written by H. Matsui on Nov., 2008
!
!      subroutine choose_para_ucd_file_format(file_fmt_ctl, i_file_fmt, &
!     &          id_field_file_format)
!      subroutine choose_ucd_file_format(file_fmt_ctl, i_file_fmt,      &
!     &          id_field_file_format)
!
! ------------------------------------------------------------------
!   flags for field data
!
!  subdomain  data file
!   ASCII   data file
!     UCD file:             ucd_ascii
!     splitted UCD file:    udt_ascii
!     VTK file:             vtk_ascii
!     splitted VTK file:    vtd_ascii
!   BINARY  data file
!     splitted file:        binary
!
!   GZIPPED data file
!     UCD file:             ucd_gzip
!     splitted UCD file:    udt_gzip
!     VTK file:             vtk_gzip
!     splitted VTK file:    vtd_gzip
!
!  whole domain data file
!   ASCII   data file
!     UCD file:             single_ucd_ascii
!     splitted UCD file:    single_udt_ascii
!     VTK file:             single_vtk_ascii
!     splitted VTK file:    single_vtd_ascii
!
!   GZIPPED data file
!     UCD file:             single_ucd_gzip
!     splitted UCD file:    single_udt_gzip
!     VTK file:             single_vtk_gzip
!     splitted VTK file:    single_vtd_gzip
!
! ------------------------------------------------------------------
!
      module m_field_file_format
!
      use m_precision
      use m_file_format_switch
!
      implicit    none
!
      integer(kind = kint), parameter :: iflag_ascii                    &
     &                     = id_ascii_file_fmt
      integer(kind = kint), parameter :: iflag_bin                      &
     &                     = id_binary_file_fmt
      integer(kind = kint), parameter :: iflag_gzip                     &
     &                     = id_gzip_txt_file_fmt
!
      integer(kind = kint), parameter :: iflag_fld =       0
      integer(kind = kint), parameter :: iflag_ucd =      10
      integer(kind = kint), parameter :: iflag_udt =      20
      integer(kind = kint), parameter :: iflag_vtk =      30
      integer(kind = kint), parameter :: iflag_vtd =      40
!
      integer(kind = kint), parameter :: iflag_para =      0
      integer(kind = kint), parameter :: iflag_single =  100
!
!
      integer(kind = kint), parameter :: iflag_sgl_ucd = 110
      integer(kind = kint), parameter :: iflag_sgl_udt = 120
      integer(kind = kint), parameter :: iflag_sgl_vtk = 130
      integer(kind = kint), parameter :: iflag_sgl_vtd = 140
!
      integer(kind = kint), parameter :: iflag_fld_gz =       3
      integer(kind = kint), parameter :: iflag_ucd_gz =      13
      integer(kind = kint), parameter :: iflag_udt_gz =      23
      integer(kind = kint), parameter :: iflag_vtk_gz =      33
      integer(kind = kint), parameter :: iflag_vtd_gz =      43
      integer(kind = kint), parameter :: iflag_sgl_ucd_gz = 113
      integer(kind = kint), parameter :: iflag_sgl_udt_gz = 123
      integer(kind = kint), parameter :: iflag_sgl_vtk_gz = 133
      integer(kind = kint), parameter :: iflag_sgl_vtd_gz = 143
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine choose_ucd_file_format(file_fmt_ctl, i_file_fmt,       &
     &          id_field_file_format)
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(inout) :: id_field_file_format
!
!
      if (i_file_fmt .eq. 0) then
        id_field_file_format = iflag_fld
        return
      end if
!
      if(     file_fmt_ctl.eq.'binary'                                  &
     &   .or. file_fmt_ctl.eq.'BINARY'                                  &
     &   .or. file_fmt_ctl.eq.'bin'                                     &
     &   .or. file_fmt_ctl.eq.'BIN') then
           id_field_file_format = iflag_udt
      else if(file_fmt_ctl.eq.'ascii'                                   &
     &   .or. file_fmt_ctl.eq.'ASCII'                                   &
     &   .or. file_fmt_ctl.eq.'fld'                                     &
     &   .or. file_fmt_ctl.eq.'FLD'                                     &
     &   .or. file_fmt_ctl.eq.'fld_ascii'                               &
     &   .or. file_fmt_ctl.eq.'FLD_ASCII'                               &
     &   .or. file_fmt_ctl.eq.'field'                                   &
     &   .or. file_fmt_ctl.eq.'FIELD'                                   &
     &   .or. file_fmt_ctl.eq.'field_ascii'                             &
     &   .or. file_fmt_ctl.eq.'FIELD_ASCII') then
           id_field_file_format = iflag_fld
      else if(file_fmt_ctl.eq.'gzip'                                    &
     &   .or. file_fmt_ctl.eq.'GZIP'                                    &
     &   .or. file_fmt_ctl.eq.'gz'                                      &
     &   .or. file_fmt_ctl.eq.'GZ'                                      &
     &   .or. file_fmt_ctl.eq.'fld_gzip'                                &
     &   .or. file_fmt_ctl.eq.'FLD_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'fld_gz'                                  &
     &   .or. file_fmt_ctl.eq.'FLD_GZ'                                  &
     &   .or. file_fmt_ctl.eq.'field_gzip'                              &
     &   .or. file_fmt_ctl.eq.'FIELD_GZIP'                              &
     &   .or. file_fmt_ctl.eq.'field_gz'                                &
     &   .or. file_fmt_ctl.eq.'FIELD_GZ') then
           id_field_file_format = iflag_fld + iflag_gzip
      else if(file_fmt_ctl.eq.'udt'                                     &
     &   .or. file_fmt_ctl.eq.'UDT'                                     &
     &   .or. file_fmt_ctl.eq.'udt_ascii'                               &
     &   .or. file_fmt_ctl.eq.'UDT_ASCII') then
           id_field_file_format = iflag_udt
      else if(file_fmt_ctl.eq.'udt_gzip'                                &
     &   .or. file_fmt_ctl.eq.'UDT_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'udt_gz'                                  &
     &   .or. file_fmt_ctl.eq.'UDT_GZ') then
           id_field_file_format = iflag_udt + iflag_gzip
!
      else if(file_fmt_ctl.eq.'ucd'                                     &
     &   .or. file_fmt_ctl.eq.'UCD'                                     &
     &   .or. file_fmt_ctl.eq.'ucd_ascii'                               &
     &   .or. file_fmt_ctl.eq.'UCD_ASCII') then
           id_field_file_format = iflag_ucd
      else if(file_fmt_ctl.eq.'ucd_gzip'                                &
     &   .or. file_fmt_ctl.eq.'UCD_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'ucd_gz'                                  &
     &   .or. file_fmt_ctl.eq.'UCD_GZ') then
           id_field_file_format = iflag_ucd + iflag_gzip
!
      else if(file_fmt_ctl.eq.'vtd'                                     &
     &   .or. file_fmt_ctl.eq.'VTD'                                     &
     &   .or. file_fmt_ctl.eq.'vtd_ascii'                               &
     &   .or. file_fmt_ctl.eq.'VTD_ASCII') then
           id_field_file_format = iflag_vtd
      else if(file_fmt_ctl.eq.'vtd_gzip'                                &
     &   .or. file_fmt_ctl.eq.'VTD_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'vtd_gz'                                  &
     &   .or. file_fmt_ctl.eq.'VTD_GZ') then
           id_field_file_format = iflag_vtd + iflag_gzip
!
      else if(file_fmt_ctl.eq.'vtk'                                     &
     &   .or. file_fmt_ctl.eq.'VTK'                                     &
     &   .or. file_fmt_ctl.eq.'vtk_ascii'                               &
     &   .or. file_fmt_ctl.eq.'VTK_ASCII') then
           id_field_file_format = iflag_vtk
      else if(file_fmt_ctl.eq.'vtk_gzip'                                &
     &   .or. file_fmt_ctl.eq.'VTK_GZIP'                                &
     &   .or. file_fmt_ctl.eq.'vtk_gz'                                  &
     &   .or. file_fmt_ctl.eq.'VTK_GZ') then
           id_field_file_format = iflag_vtk + iflag_gzip
      else
           id_field_file_format = iflag_udt
      end if
!
      end subroutine choose_ucd_file_format
!
! -----------------------------------------------------------------------
!
      subroutine choose_para_ucd_file_format(file_fmt_ctl, i_file_fmt,  &
     &          id_field_file_format)
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(inout) :: id_field_file_format
!
!
      if (i_file_fmt .eq. 0) then
        id_field_file_format = iflag_udt
        return
      end if
!
      if(file_fmt_ctl.eq.'single'                                       &
     &   .or. file_fmt_ctl.eq.'SINGLE'                                  &
     &   .or. file_fmt_ctl.eq.'merged'                                  &
     &   .or. file_fmt_ctl.eq.'MERGED'                                  &
     &   .or. file_fmt_ctl.eq.'single_ascii'                            &
     &   .or. file_fmt_ctl.eq.'SINGLE_ASCII'                            &
     &   .or. file_fmt_ctl.eq.'single_udt'                              &
     &   .or. file_fmt_ctl.eq.'SINGLE_UDT'                              &
     &   .or. file_fmt_ctl.eq.'single_udt_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_UDT_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_ascii'                            &
     &   .or. file_fmt_ctl.eq.'MERGED_ASCII'                            &
     &   .or. file_fmt_ctl.eq.'merged_udt'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_UDT'                              &
     &   .or. file_fmt_ctl.eq.'merged_udt_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_UDT_ASCII') then
           id_field_file_format = iflag_sgl_udt
      else if(file_fmt_ctl.eq.'single_gzip'                             &
     &   .or. file_fmt_ctl.eq.'SINGLE_GZIP'                             &
     &   .or. file_fmt_ctl.eq.'single_udt_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_UDT_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_gzip'                             &
     &   .or. file_fmt_ctl.eq.'MERGED_GZIP'                             &
     &   .or. file_fmt_ctl.eq.'merged_udt_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_UDT_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'single_gz'                               &
     &   .or. file_fmt_ctl.eq.'SINGLE_GZ'                               &
     &   .or. file_fmt_ctl.eq.'single_udt_gz'                           &
     &   .or. file_fmt_ctl.eq.'SINGLE_UDT_GZ'                           &
     &   .or. file_fmt_ctl.eq.'merged_gz'                               &
     &   .or. file_fmt_ctl.eq.'MERGED_GZ'                               &
     &   .or. file_fmt_ctl.eq.'merged_udt_gz'                           &
     &   .or. file_fmt_ctl.eq.'MERGED_UDT_GZ') then
           id_field_file_format = iflag_sgl_udt + iflag_gzip
!
      else if(file_fmt_ctl.eq.'single_ucd'                              &
     &   .or. file_fmt_ctl.eq.'SINGLE_UCD'                              &
     &   .or. file_fmt_ctl.eq.'single_ucd_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_UCD_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_ucd'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_UCD'                              &
     &   .or. file_fmt_ctl.eq.'merged_ucd_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_UCD_ASCII') then
           id_field_file_format = iflag_sgl_ucd
      else if(file_fmt_ctl.eq.'single_ucd_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_UCD_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_ucd_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_UCD_GZIP') then
           id_field_file_format = iflag_sgl_ucd + iflag_gzip
!
      else if(file_fmt_ctl.eq.'single_vtd'                              &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTD'                              &
     &   .or. file_fmt_ctl.eq.'single_vtd_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTD_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_vtd'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_VTD'                              &
     &   .or. file_fmt_ctl.eq.'merged_vtd_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_VTD_ASCII') then
           id_field_file_format = iflag_sgl_vtd
      else if(file_fmt_ctl.eq.'single_vtd_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTD_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_vtd_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_VTD_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'single_vtd_gz'                           &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTD_GZ'                           &
     &   .or. file_fmt_ctl.eq.'merged_vtd_gz'                           &
     &   .or. file_fmt_ctl.eq.'MERGED_VTD_GZ') then
           id_field_file_format = iflag_sgl_vtd + iflag_gzip
!
      else if(file_fmt_ctl.eq.'single_vtk'                              &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK'                              &
     &   .or. file_fmt_ctl.eq.'single_vtk_ascii'                        &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_ASCII'                        &
     &   .or. file_fmt_ctl.eq.'merged_vtk'                              &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK'                              &
     &   .or. file_fmt_ctl.eq.'merged_vtk_ascii'                        &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_ASCII') then
           id_field_file_format = iflag_sgl_vtk
      else if(file_fmt_ctl.eq.'single_vtk_gzip'                         &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'merged_vtk_gzip'                         &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_GZIP'                         &
     &   .or. file_fmt_ctl.eq.'single_vtk_gz'                           &
     &   .or. file_fmt_ctl.eq.'SINGLE_VTK_GZ'                           &
     &   .or. file_fmt_ctl.eq.'merged_vtk_gz'                           &
     &   .or. file_fmt_ctl.eq.'MERGED_VTK_GZ') then
           id_field_file_format = iflag_sgl_vtk + iflag_gzip
      else
        call choose_ucd_file_format(file_fmt_ctl, i_file_fmt,           &
     &          id_field_file_format)
      end if
!
      end subroutine choose_para_ucd_file_format
!
! -----------------------------------------------------------------------
!
      end module m_field_file_format
