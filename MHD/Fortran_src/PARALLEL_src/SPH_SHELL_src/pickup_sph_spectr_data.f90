!>@file   pickup_sph_spectr_data.f90
!!@brief  module pickup_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine init_sph_spec_4_monitor(sph_params, sph_rj, rj_fld,  &
!!     &          pick_list, picked)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(pickup_mode_list), intent(inout) :: pick_list
!!        type(picked_spectrum_data), intent(inout) :: picked
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module pickup_sph_spectr_data
!
      use m_precision
      use m_constants
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_spec_4_monitor(sph_params, sph_rj, rj_fld,    &
     &          pick_list, picked)
!
      use calypso_mpi
      use quicksort
!
      use t_phys_data
      use pickup_sph_coefs
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: l, num
      integer(kind = kint) :: iflag_center = 0
!
!
      num = pick_list%num_modes                                         &
     &     + pick_list%num_degree + pick_list%num_order
      if(num .eq. 0) return
!
      call init_sph_radial_monitor_list(sph_rj, picked, iflag_center)
!
      call count_sph_labels_4_monitor(rj_fld%num_phys,                  &
     &    rj_fld%num_component, rj_fld%iflag_monitor, picked)
!
      if(pick_list%num_degree .eq. -9999) then
        pick_list%num_degree = sph_params%l_truncation+ 1 
        call alloc_pick_sph_l(pick_list)
        do l = 0, sph_params%l_truncation
          pick_list%idx_pick_l(l+1) = l
        end do
      end if
!
      write(*,*) 'const_picked_sph_address for picked_spectr'
      call const_picked_sph_address(iflag_center,                       &
     &    sph_params%l_truncation, sph_rj, pick_list, picked)
!
      call set_sph_fld_id_4_monitor(rj_fld%num_phys,                    &
     &    rj_fld%num_component, rj_fld%iflag_monitor, picked)
!
      if(my_rank .ne. 0) return
      call set_sph_labels_4_monitor                                     &
     &   (rj_fld%num_phys, rj_fld%num_component,                        &
     &    rj_fld%phys_name, picked)
!
      end subroutine init_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_spectr_data
