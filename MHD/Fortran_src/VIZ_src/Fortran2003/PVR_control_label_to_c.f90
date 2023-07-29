!>@file   PVR_control_label_to_c.f90
!!@brief  module PVR_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for volume rendering
!!
!!@verbatim
!!      integer(c_int) function num_flag_pvr_movie_mode_f() bind(c)
!!      subroutine set_flag_pvr_movie_mode_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_flag_pvr_isosurf_dir_f() bind(c)
!!      subroutine set_flag_pvr_isosurf_dir_f(names_c)  bind(c)
!!!!
!!      integer(c_int) function num_flag_LIC_movie_mode_f()             &
!!     &              bind(c, name="num_flag_LIC_movie_mode_f")
!!      subroutine set_flag_LIC_movie_mode_f(labels_c)                  &
!!     &          bind(c, name="set_flag_LIC_movie_mode_f")
!!@endverbatim
!
      module PVR_control_label_to_c
!
      use ISO_C_BINDING
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_pvr_movie_mode_f() bind(c)
!
      use t_control_params_4_pvr
!
      num_flag_pvr_movie_mode_f = num_flag_pvr_movie_mode()
      return
      end function num_flag_pvr_movie_mode_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_flag_pvr_movie_mode_f(names_c)  bind(c)
!
      use t_control_params_4_pvr
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_flag_pvr_movie_mode()])
      call set_flag_pvr_movie_mode(name_f)
      end subroutine set_flag_pvr_movie_mode_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_pvr_isosurf_dir_f() bind(c)
!
      use pvr_surface_enhancement
!
      num_flag_pvr_isosurf_dir_f = num_flag_pvr_isosurf_dir()
      return
      end function num_flag_pvr_isosurf_dir_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_flag_pvr_isosurf_dir_f(names_c)  bind(c)
!
      use pvr_surface_enhancement
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_flag_pvr_isosurf_dir()])
      call set_flag_pvr_isosurf_dir(name_f)
      end subroutine set_flag_pvr_isosurf_dir_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_LIC_movie_mode_f()               &
     &              bind(c, name="num_flag_LIC_movie_mode_f")
!
      use t_control_params_4_pvr
!
      num_flag_LIC_movie_mode_f = num_flag_LIC_movie_mode()
      return
      end function num_flag_LIC_movie_mode_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_flag_LIC_movie_mode_f(labels_c)                    &
     &          bind(c, name="set_flag_LIC_movie_mode_f")
!
      use t_control_params_4_pvr
!
      type(C_ptr), value :: labels_c
!
      character(len=kchara), pointer :: labels(:)
!
      call c_f_pointer(labels_c, labels, [num_flag_LIC_movie_mode()])
      call set_flag_LIC_movie_mode(labels)
      end subroutine set_flag_LIC_movie_mode_f
!
! ----------------------------------------------------------------------
!
      end module PVR_control_label_to_c
