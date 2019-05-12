!>@file   pickup_sph_rms_spectr.f90
!!@brief      module pickup_sph_rms_spectr
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief Select mean square data to output
!!
!!@verbatim
!!      subroutine allocate_work_pick_rms_sph(nri, jmax)
!!      subroutine init_sph_rms_4_monitor                               &
!!     &         (sph_params, sph_rj, pwr, pick_list, pick_rms)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(picked_spectrum_data), intent(inout) :: pick_rms
!!@endverbatim
!
      module pickup_sph_rms_spectr
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use t_phys_data
      use t_schmidt_poly_on_rtm
      use t_phys_address
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_pickup_sph_spectr_data
!
      use pickup_sph_spectr
!
      implicit  none
!
      real(kind = kreal), allocatable :: rms_sph_rj(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_v(:,:)
!
      private :: rms_sph_rj, rms_sph_v
!
      private :: set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_work_pick_rms_sph(nri, jmax)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
!
      allocate( rms_sph_rj(0:nri,jmax,3) )
      allocate( rms_sph_v(jmax,3) )
!
      rms_sph_rj =  0.0d0
      rms_sph_v =  0.0d0
!
      end subroutine allocate_work_pick_rms_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_work_pick_rms_sph
!
!
      deallocate( rms_sph_rj, rms_sph_v)
!
      end subroutine deallocate_work_pick_rms_sph
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_rms_4_monitor                                 &
     &         (sph_params, sph_rj, pwr, pick_list, pick_rms)
!
      use pickup_sph_coefs
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_mean_squares), intent(in) :: pwr
!
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: pick_rms
!
      integer(kind = kint) :: iflag_center = 0
!
!
      call init_sph_radial_monitor_list(sph_rj, pick_rms, iflag_center)
!
      call const_picked_sph_address(iflag_center,                       &
     &    sph_params%l_truncation, sph_rj, pick_list, pick_rms)
!
      call set_sph_rms_labels_4_monitor(pwr, pick_rms)
!
      end subroutine init_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_rms_labels_4_monitor(pwr, pick_rms)
!
      use m_phys_labels
      use add_direction_labels
      use sph_mean_spectr_header_IO
!
      type(sph_mean_squares), intent(in) :: pwr
      type(picked_spectrum_data), intent(inout) :: pick_rms
!
      integer(kind = kint) :: i_fld, ist, ncomp
!
!
      do i_fld = 1, pwr%num_fld_sq
        ist =   pwr%istack_comp_sq(i_fld-1)
        ncomp = pwr%num_comp_sq(i_fld)
        call set_sph_rms_labels(ncomp, pwr%pwr_name(i_fld),             &
     &      pick_rms%spectr_name(ist+1:ist+ncomp))
      end do
      pick_rms%ntot_comp_rj = pwr%ntot_comp_sq
!
      end subroutine set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_rms_spectr
