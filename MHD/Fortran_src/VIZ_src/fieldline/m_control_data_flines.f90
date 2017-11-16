!
!      module m_control_data_flines
!
!      Written by H. Matsui on July, 2006
!
!      subroutine allocate_fline_ctl_struct
!      subroutine deallocate_fline_fhead_ctl
!
!      subroutine read_files_4_fline_ctl
!      subroutine bcast_files_4_fline_ctl
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  fieldline  1
!!      file  fieldline  'ctl_fline_magne'
!!    end array fieldline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_control_data_flines
!
      use m_precision
      use t_control_data_flines
!
      implicit  none
!
!
      type(fieldline_controls), save :: fline_ctls1
!
      end module m_control_data_flines
