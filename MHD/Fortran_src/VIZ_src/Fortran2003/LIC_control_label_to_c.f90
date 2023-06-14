!>@file   LIC_control_label_to_c.f90
!!        module LIC_control_label_to_c
!!
!! @author H. Matsui
!! @date   Programmed in May., 2020
!!
!!
!> @brief Routines to tell control labels into C programs
!!
!!@verbatim
!!      integer(c_int) function num_ctl_label_LIC_pvr_f()               &
!!     &              bind(c, name="num_ctl_label_LIC_pvr_f")
!!      subroutine set_ctl_label_LIC_pvr_f(labels_c)                    &
!!     &          bind(c, name="set_ctl_label_LIC_pvr_f")
!!
!!      integer(c_int) function num_ctl_label_LIC_f()                   &
!!     &              bind(c, name="num_ctl_label_LIC_f")
!!      subroutine set_ctl_label_LIC_f(labels_c)                        &
!!     &          bind(c, name="set_ctl_label_LIC_f")
!!
!!      integer(c_int) function num_ctl_label_LIC_noise_f()             &
!!     &              bind(c, name="num_ctl_label_LIC_noise_f")
!!      subroutine set_ctl_label_LIC_noise_f(labels_c)                  &
!!     &          bind(c, name="set_ctl_label_LIC_noise_f")
!!
!!      integer(c_int) function num_ctl_label_LIC_kernel_f()            &
!!     &              bind(c, name="num_ctl_label_LIC_kernel_f")
!!      subroutine set_ctl_label_LIC_kernel_f(labels_c)                 &
!!     &          bind(c, name="set_ctl_label_LIC_kernel_f")
!!
!!      integer(c_int) function num_ctl_label_masking_f()               &
!!     &              bind(c, name="num_ctl_label_masking_f")
!!      subroutine set_ctl_label_masking_f(labels_c)                    &
!!     &          bind(c, name="set_ctl_label_masking_f")
!!
!!      integer(c_int) function num_label_LIC_colormap_f()              &
!!     &          bind(c, name="num_label_LIC_colormap_f")
!!      subroutine set_label_LIC_colormap_f(names_c)                    &
!!     &          bind(c, name="set_label_LIC_colormap_f")
!!
!!      integer(c_int) function num_label_LIC_movie_f()                 &
!!     &              bind(c, name="num_label_LIC_movie_f")
!!      subroutine set_label_LIC_movie_f(labels_c)                      &
!!     &          bind(c, name="set_label_LIC_movie_f")
!!
!!      integer(c_int) function num_flag_LIC_movie_mode_f()             &
!!     &              bind(c, name="num_flag_LIC_movie_mode_f")
!!      subroutine set_flag_LIC_movie_mode_f(labels_c)                  &
!!     &          bind(c, name="set_flag_LIC_movie_mode_f")
!!@endverbatim
!
      module LIC_control_label_to_c
!
!
      use m_precision
      use ISO_C_BINDING
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_ctl_label_LIC_pvr_f()                 &
     &              bind(c, name="num_ctl_label_LIC_pvr_f")
!
      use ctl_data_lic_pvr_IO
!
      num_ctl_label_LIC_pvr_f = num_ctl_label_LIC_pvr()
!
      end function num_ctl_label_LIC_pvr_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_pvr_f(labels_c)                      &
     &          bind(c, name="set_ctl_label_LIC_pvr_f")
!
      use ctl_data_lic_pvr_IO
!
      type(C_ptr), value :: labels_c
!
      character(len=kchara), pointer :: labels(:)
!
      call c_f_pointer(labels_c, labels, [num_ctl_label_LIC_pvr()])
      call set_ctl_label_LIC_pvr(labels)
!
      end subroutine set_ctl_label_LIC_pvr_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_ctl_label_LIC_f()                     &
     &              bind(c, name="num_ctl_label_LIC_f")
!
      use ctl_data_LIC_IO
!
      num_ctl_label_LIC_f = num_ctl_label_LIC()
!
      end function num_ctl_label_LIC_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_f(labels_c)                          &
     &          bind(c, name="set_ctl_label_LIC_f")
!
      use ctl_data_LIC_IO
!
      type(C_ptr), value :: labels_c
!
      character(len=kchara), pointer :: labels(:)
!
      call c_f_pointer(labels_c, labels, [num_ctl_label_LIC()])
      call set_ctl_label_LIC(labels)
!
      end subroutine set_ctl_label_LIC_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_ctl_label_LIC_noise_f()               &
     &              bind(c, name="num_ctl_label_LIC_noise_f")
!
      use t_control_data_LIC_noise
!
      num_ctl_label_LIC_noise_f = num_ctl_label_LIC_noise()
!
      end function num_ctl_label_LIC_noise_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_noise_f(labels_c)                    &
     &          bind(c, name="set_ctl_label_LIC_noise_f")
!
      use t_control_data_LIC_noise
!
      type(C_ptr), value :: labels_c
!
      character(len=kchara), pointer :: labels(:)
!
      call c_f_pointer(labels_c, labels, [num_ctl_label_LIC_noise()])
      call set_ctl_label_LIC_noise(labels)
!
      end subroutine set_ctl_label_LIC_noise_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_ctl_label_LIC_kernel_f()              &
     &              bind(c, name="num_ctl_label_LIC_kernel_f")
!
      use t_control_data_LIC_kernel
!
      num_ctl_label_LIC_kernel_f = num_ctl_label_LIC_kernel()
!
      end function num_ctl_label_LIC_kernel_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_kernel_f(labels_c)                   &
     &          bind(c, name="set_ctl_label_LIC_kernel_f")
!
      use t_control_data_LIC_kernel
!
      type(C_ptr), value :: labels_c
!
      character(len=kchara), pointer :: labels(:)
!
      call c_f_pointer(labels_c, labels, [num_ctl_label_LIC_kernel()])
      call set_ctl_label_LIC_kernel(labels)
!
      end subroutine set_ctl_label_LIC_kernel_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_ctl_label_masking_f()                 &
     &              bind(c, name="num_ctl_label_masking_f")
!
      use t_control_data_masking
!
      num_ctl_label_masking_f = num_ctl_label_masking()
!
      end function num_ctl_label_masking_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_masking_f(labels_c)                      &
     &          bind(c, name="set_ctl_label_masking_f")
!
      use t_control_data_masking
!
      type(C_ptr), value :: labels_c
!
      character(len=kchara), pointer :: labels(:)
!
      call c_f_pointer(labels_c, labels, [num_ctl_label_masking()])
      call set_ctl_label_masking(labels)
!
      end subroutine set_ctl_label_masking_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_LIC_colormap_f()                &
     &          bind(c, name="num_label_LIC_colormap_f")
!
      use ctl_data_pvr_colormap_IO
!
      num_label_LIC_colormap_f = num_label_LIC_colormap()
      return
      end function num_label_LIC_colormap_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_LIC_colormap_f(names_c)                      &
     &          bind(c, name="set_label_LIC_colormap_f")
!
      use ctl_data_pvr_colormap_IO
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: names(:)
!
      call c_f_pointer(names_c, names, [num_label_LIC_colormap()])
      call set_label_LIC_colormap(names)
      end subroutine set_label_LIC_colormap_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_LIC_movie_f()                   &
     &              bind(c, name="num_label_LIC_movie_f")
!
      use ctl_data_pvr_movie_IO
!
      num_label_LIC_movie_f = num_label_LIC_movie()
!
      end function num_label_LIC_movie_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_LIC_movie_f(labels_c)                        &
     &          bind(c, name="set_label_LIC_movie_f")
!
      use ctl_data_pvr_movie_IO
!
      type(C_ptr), value :: labels_c
!
      character(len=kchara), pointer :: labels(:)
!
      call c_f_pointer(labels_c, labels, [num_label_LIC_movie()])
      call set_label_LIC_movie(labels)
!
      end subroutine set_label_LIC_movie_f
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
      end module LIC_control_label_to_c
