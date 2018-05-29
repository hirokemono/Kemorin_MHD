!
      module calypso_c_binding
!
      use ISO_C_BINDING
!
!  ---------------------------------------------------------------------
!
      interface
!
!  ---------------------------------------------------------------------
!
      subroutine open_wt_rawfile(file_name, ierr)                       &
     &          BIND(C, name = 'open_wt_rawfile')
      use ISO_C_BINDING
        character(*) :: file_name
        integer(C_INT) :: ierr
      end subroutine open_wt_rawfile
!
      subroutine open_ad_rawfile(file_name, ierr)                       &
     &          BIND(C, name = 'open_ad_rawfile')
      use ISO_C_BINDING
        character(*) :: file_name
        integer(C_INT) :: ierr
      end subroutine open_ad_rawfile
!
      subroutine open_rd_rawfile(file_name, ierr)                       &
     &          BIND(C, name = 'open_rd_rawfile')
      use ISO_C_BINDING
        character(*) :: file_name
        integer(C_INT) :: ierr
      end subroutine open_rd_rawfile
!
      subroutine close_rawfile()  BIND(C, name = 'close_rawfile')
      end subroutine
!
      subroutine rawread_f(iflag_swap, ilength, textbuf, lenchara)      &
     &          BIND(C, name = 'rawread_f')
      use ISO_C_BINDING
        integer(C_INT) :: iflag_swap
        integer(C_INT) :: ilength
        character(*) :: textbuf
        integer(C_INT) :: lenchara
      end subroutine rawread_f
!
      subroutine rawwrite_f(length, textbuf, lenchara)                  &
     &          BIND(C, name = 'rawwrite_f')
      use ISO_C_BINDING
        integer(C_INT) :: ilength
        character(*) :: textbuf
        integer(C_INT) :: lenchara
      end subroutine rawwrite_f
!
!
      subroutine open_wt_gzfile(gz_file_name)                           &
     &          BIND(C, name = 'open_wt_gzfile')
      use ISO_C_BINDING
        character(*) :: gz_file_name
      end subroutine open_wt_gzfile
!
      subroutine open_ad_gzfile(gz_file_name)                           &
     &          BIND(C, name = 'open_ad_gzfile')
      use ISO_C_BINDING
        character(*) :: gz_file_name
      end subroutine open_ad_gzfile
!
      subroutine open_rd_gzfile(gz_file_name)                           &
     &          BIND(C, name = 'open_rd_gzfile')
      use ISO_C_BINDING
        character(*) :: gz_file_name
      end subroutine open_rd_gzfile
!
      subroutine close_gzfile()  BIND(C, name = 'close_gzfile')
      end subroutine close_gzfile
!
      subroutine check_gzfile_eof()  BIND(C, name = 'check_gzfile_eof')
      end subroutine check_gzfile_eof
!
      end interface
!
      end module calypso_c_binding
