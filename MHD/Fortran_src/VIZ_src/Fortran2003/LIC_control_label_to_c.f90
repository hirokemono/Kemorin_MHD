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
!!      integer(c_int) function num_ctl_label_LIC_f()                   &
!!     &              bind(c, name="num_ctl_label_LIC_f")
!!      subroutine set_ctl_label_LIC_f(labels)                          &
!!     &          bind(c, name="set_ctl_label_LIC_f")
!!
!!      integer(c_int) function num_ctl_label_LIC_pvr_f()               &
!!     &              bind(c, name="num_ctl_label_LIC_pvr_f")
!!      subroutine set_ctl_label_LIC_pvr_f(labels)                      &
!!     &          bind(c, name="set_ctl_label_LIC_pvr_f")
!!
!!      integer(c_int) function num_ctl_label_LIC_noise_f()             &
!!     &              bind(c, name="num_ctl_label_LIC_noise_f")
!!      subroutine set_ctl_label_LIC_noise_f(labels)                    &
!!     &          bind(c, name="set_ctl_label_LIC_noise_f")
!!
!!      integer(c_int) function num_ctl_label_LIC_kernel_f()            &
!!     &              bind(c, name="num_ctl_label_LIC_kernel_f")
!!      subroutine set_ctl_label_LIC_kernel_f(labels)                   &
!!     &          bind(c, name="set_ctl_label_LIC_kernel_f")
!!
!!      integer(c_int) function num_ctl_label_LIC_masking_f()           &
!!     &              bind(c, name="num_ctl_label_LIC_masking_f")
!!      subroutine set_ctl_label_LIC_masking_f(labels)                  &
!!     &          bind(c, name="set_ctl_label_LIC_masking_f")
!!
!!      integer(c_int) function num_label_LIC_colormap_f()              &
!!     &          bind(c, name="num_label_LIC_colormap_f")
!!      subroutine set_label_LIC_colormap_f(names)                      &
!!     &          bind(c, name="set_label_LIC_colormap_f")
!!
!!      integer(c_int) function num_label_LIC_movie_f()                 &
!!     &              bind(c, name="num_label_LIC_movie_f")
!!      subroutine set_label_LIC_movie_f(labels)                        &
!!     &          bind(c, name="set_label_LIC_movie_f")
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
      integer(c_int) function num_ctl_label_LIC_f()                     &
     &              bind(c, name="num_ctl_label_LIC_f")
!
      use t_control_data_lic
!
      num_ctl_label_LIC_f = num_ctl_label_LIC()
!
      end function num_ctl_label_LIC_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_f(labels)                            &
     &          bind(c, name="set_ctl_label_LIC_f")
!
      use t_control_data_lic
!
      character(C_CHAR), intent(inout) :: labels(*)
!
      call set_ctl_label_LIC(labels(1))
!
      end subroutine set_ctl_label_LIC_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_ctl_label_LIC_pvr_f()                 &
     &              bind(c, name="num_ctl_label_LIC_pvr_f")
!
      use t_control_data_lic_pvr
!
      num_ctl_label_LIC_pvr_f = num_ctl_label_LIC_pvr()
!
      end function num_ctl_label_LIC_pvr_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_pvr_f(labels)                        &
     &          bind(c, name="set_ctl_label_LIC_pvr_f")
!
      use t_control_data_lic_pvr
!
      character(C_CHAR), intent(inout) :: labels(*)
!
      call set_ctl_label_LIC_pvr(labels(1))
!
      end subroutine set_ctl_label_LIC_pvr_f
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
      subroutine set_ctl_label_LIC_noise_f(labels)                      &
     &          bind(c, name="set_ctl_label_LIC_noise_f")
!
      use t_control_data_LIC_noise
!
      character(C_CHAR), intent(inout) :: labels(*)
!
      call set_ctl_label_LIC_noise(labels(1))
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
      subroutine set_ctl_label_LIC_kernel_f(labels)                     &
     &          bind(c, name="set_ctl_label_LIC_kernel_f")
!
      use t_control_data_LIC_kernel
!
      character(C_CHAR), intent(inout) :: labels(*)
!
      call set_ctl_label_LIC_kernel(labels(1))
!
      end subroutine set_ctl_label_LIC_kernel_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_ctl_label_LIC_masking_f()             &
     &              bind(c, name="num_ctl_label_LIC_masking_f")
!
      use t_control_data_LIC_masking
!
      num_ctl_label_LIC_masking_f = num_ctl_label_LIC_masking()
!
      end function num_ctl_label_LIC_masking_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_masking_f(labels)                    &
     &          bind(c, name="set_ctl_label_LIC_masking_f")
!
      use t_control_data_LIC_masking
!
      character(C_CHAR), intent(inout) :: labels(*)
!
      call set_ctl_label_LIC_masking(labels(1))
!
      end subroutine set_ctl_label_LIC_masking_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_LIC_colormap_f()                &
     &          bind(c, name="num_label_LIC_colormap_f")
!
      use t_ctl_data_pvr_colormap
!
      num_label_LIC_colormap_f = num_label_LIC_colormap()
      return
      end function num_label_LIC_colormap_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_LIC_colormap_f(names)                        &
     &          bind(c, name="set_label_LIC_colormap_f")
!
      use t_ctl_data_pvr_colormap
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_LIC_colormap(names)
      end subroutine set_label_LIC_colormap_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_LIC_movie_f()                   &
     &              bind(c, name="num_label_LIC_movie_f")
!
      use t_control_data_pvr_movie
!
      num_label_LIC_movie_f = num_label_LIC_movie()
!
      end function num_label_LIC_movie_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_LIC_movie_f(labels)                          &
     &          bind(c, name="set_label_LIC_movie_f")
!
      use t_control_data_pvr_movie
!
      character(C_CHAR), intent(inout) :: labels(*)
!
      call set_label_LIC_movie(labels(1))
!
      end subroutine set_label_LIC_movie_f
!
! ----------------------------------------------------------------------
!
      end module LIC_control_label_to_c
