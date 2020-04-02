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
      end interface
!
      end module calypso_c_binding
