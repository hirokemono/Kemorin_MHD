!>@file   m_volume_average_labels.f90
!!@brief  module m_volume_average_labels
!!
!!@author H. Matsui
!!@date Programmed in June, 2009
!
!>@brief  Set control parameters for spherical harmonics dynamo from IO
!!
!!@verbatim
!!      subroutine set_vector_label(fid_label, label_v)
!!      subroutine set_sym_tensor_label(fid_label, label_st)
!!      subroutine set_asym_tensor_label(fid_label, label_ast)
!!@endverbatim
!
!
      module m_volume_average_labels
!
      use m_precision
!
      use m_phys_labels
!
      implicit none
!
!
!>        Field label for volumw
      character(len=kchara), parameter :: e_hd_volume = 'Volume'
!
!
!>        Field label for kinetic enegy
      character(len=kchara), parameter :: e_hd_k_ene = 'K_ene'
!>        Field label for divergence of velocity
      character(len=kchara), parameter :: e_hd_div_v = 'div_v'
!
!>        Field label for angular momentum
      character(len=kchara), parameter :: e_hd_lvec(3)                  &
     &            = (/'L_x', 'L_y', 'L_z'/)
!
!
!>        Field label for magnetic enegy
      character(len=kchara), parameter :: e_hd_m_ene =    'M_ene'
!>        Field label for magnetic enegy in conductor
      character(len=kchara), parameter :: e_hd_m_ene_cd = 'M_ene_cd'
!>        Field label for divergence of magnetic field
      character(len=kchara), parameter :: e_hd_div_b =    'div_B'
!
!>        Field label for magnetic field in conductor
      character(len=kchara), parameter :: e_hd_bvec_cd(3)               &
     &            = (/'Bx_cd', 'By_cd', 'Bz_cd'/)
!
!>        Field label for divergence of vector potential
      character(len=kchara), parameter :: e_hd_div_a =    'div_A'
!
!
!>        Field label for root mean square of vorticity
      character(len=kchara), parameter :: e_hd_rms_w = 'RMS_omega'
!
!>        Field label for current density in conductor
      character(len=kchara), parameter :: e_hd_sq_j_cd =  'J_cd'
!>        Field label for root mean square of current dencity
      character(len=kchara), parameter :: e_hd_rms_j =    'RMS_J'
!>        Field label for RMS of current density in conductor
      character(len=kchara), parameter :: e_hd_rms_j_cd = 'RMS_J_cd'
!
!
!>        Field label for filtered kinetic enegy
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_k_ene = 'filter_K_ene'
!>        Field label for divergence of filtered velocity
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_v = 'div_filter_v'
!
!>        Field label for filtered angular momentum
      character(len=kchara), parameter :: e_hd_fil_lvec(3)              &
     &            = (/'filter_L_x', 'filter_L_y', 'filter_L_z'/)
!
!
!>        Field label for filtered magnetic enegy
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_m_ene =    'filter_M_ene'
!>        Field label for filtered magnetic enegy in conductor
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_m_ene_cd = 'filter_M_ene_cd'
!>        Field label for divergence of filtered magnetic field
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_b =    'div_filter_B'
!
!>        Field label for filtered magnetic field in conductor
      character(len=kchara), parameter :: e_hd_fil_bvec_cd(3)           &
     &            = (/'filter_Bx_cd','filter_By_cd','filter_Bz_cd'/)
!
!
!>        Field label for divergence of filtered vector potential
      character(len=kchara), parameter                                  &
     &                      :: e_hd_fil_div_a =    'div_filter_A'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_vector_label(fid_label, label_v)
!
      use add_direction_labels
!
      character(len=kchara), intent(in) :: fid_label
      character(len=kchara), intent(inout) :: label_v(3)
!
      call add_vector_direction_label_xyz(fid_label,                    &
     &    label_v(1), label_v(2), label_v(3) )
!
      end subroutine set_vector_label
!
! ----------------------------------------------------------------------
!
      subroutine set_sym_tensor_label(fid_label, label_st)
!
      use add_direction_labels
!
      character(len=kchara), intent(in) :: fid_label
      character(len=kchara), intent(inout) :: label_st(6)
!
      call add_tensor_direction_label_xyz(fid_label,                    &
     &    label_st(1), label_st(2), label_st(3),                        &
     &    label_st(4), label_st(5), label_st(6)  )
!
      end subroutine set_sym_tensor_label
!
! ----------------------------------------------------------------------
!
      subroutine set_asym_tensor_label(fid_label, label_ast)
!
      use add_direction_labels
!
      character(len=kchara), intent(in) :: fid_label
      character(len=kchara), intent(inout) :: label_ast(3)
!
      call add_asym_tensor_dir_label_xyz(fid_label,                     &
     &    label_ast(1), label_ast(2), label_ast(3) )
!
      end subroutine set_asym_tensor_label
!
! ----------------------------------------------------------------------
!
      end module m_volume_average_labels
