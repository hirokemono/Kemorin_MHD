!SPH_analyzer_zm_energies.f90
!     module SPH_analyzer_zm_energies
!
!      Written by H. Matsui
!
!!      subroutine SPH_analyze_zm_energies                              &
!!     &         (i_step, geofem, SPH_MHD, SPH_STR, t_IO, nod_fld)
!!        type(mesh_data), intent(in) :: geofem
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
!!        type(time_data), intent(inout) :: t_IO
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine set_ctl_data_4_zm_energies(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!
      module SPH_analyzer_zm_energies
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_SPH_data_4_SPH_trans
      use m_solver_SR
!
      implicit none
!
      private :: cal_zm_energy_to_pressure
      private :: set_rj_phys_for_convective_kene
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zm_energies                                &
     &         (i_step, geofem, SPH_MHD, SPH_STR, t_IO, nod_fld)
!
      use t_phys_address
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
!
      use field_IO_select
      use r_interpolate_sph_data
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use cal_zonal_mean_sph_spectr
!
!
      integer(kind = kint), intent(in) :: i_step
      type(mesh_data), intent(in) :: geofem
!
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(SPH_for_SPH_transforms), intent(inout) :: SPH_STR
      type(time_data), intent(inout) :: t_IO
      type(phys_data), intent(inout) :: nod_fld
!
!
!   Input spectr data
      if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
      call sel_read_step_SPH_field_file(nprocs, my_rank, i_step,        &
     &    SPH_STR%fst_file_IO, t_IO, SPH_STR%fld_IO)
!
!    copy and extend magnetic field to outside
!
      if(SPH_STR%org_rj_file_IO%iflag_IO .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO(SPH_STR%fld_IO, SPH_MHD%fld)
      else
        if (iflag_debug.gt.0) write(*,*)                                &
     &                      'r_interpolate_sph_fld_from_IO'
        call r_interpolate_sph_fld_from_IO                              &
     &     (SPH_STR%fld_IO, SPH_MHD%sph%sph_rj,                         &
     &      SPH_MHD%ipol, SPH_MHD%fld)
      end if
!
!        call set_rj_phys_for_pol_kene(SPH_MHD%sph%sph_rj, SPH_MHD%fld)
!
      call set_rj_phys_for_convective_kene(SPH_MHD%sph, SPH_MHD%fld)
!
!  spherical transform for vector
      call sph_b_trans_all_field                                        &
     &   (SPH_MHD%sph, SPH_MHD%comms, geofem%mesh,                      &
     &    SPH_STR%trans_p, SPH_STR%fld_rtp, SPH_MHD%fld,                &
     &    nod_fld, SPH_STR%WK_leg, SPH_STR%WK_FFTs, SR_sig1, SR_r1)
      call cal_zm_energy_to_pressure                                    &
     &   (SPH_MHD%sph%sph_rtp%nidx_rtp, nod_fld%n_point,                &
     &    nod_fld%num_phys, nod_fld%ntot_phys,                          &
     &    nod_fld%istack_component, SPH_MHD%fld, nod_fld%d_fld)
!
      end subroutine SPH_analyze_zm_energies
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_energies(field_ctl)
!
      use t_control_array_character3
      use m_base_field_labels
!
      type(ctl_array_c3), intent(inout) :: field_ctl
      integer(kind = kint) :: ifld, iflag_velo
!
      iflag_velo =  0
      do ifld = 1, field_ctl%num
        if(field_ctl%c1_tbl(ifld) .eq. velocity%name)  iflag_velo =  2
      end do
!
      call dealloc_control_array_c3(field_ctl)
!
      field_ctl%num = iflag_velo
      call alloc_control_array_c3(field_ctl)
!
      ifld = 0
      if(iflag_velo .gt. 0) then
        ifld = ifld+1
        field_ctl%c1_tbl(ifld) = velocity%name
        field_ctl%c2_tbl(ifld) = 'Viz_On'
        field_ctl%c3_tbl(ifld) = 'Monitor_Off'
!
        ifld = ifld+1
        field_ctl%c1_tbl(ifld) = pressure%name
        field_ctl%c2_tbl(ifld) = 'Viz_On'
        field_ctl%c3_tbl(ifld) = 'Monitor_Off'
      end if
!
      end subroutine set_ctl_data_4_zm_energies
!
! ----------------------------------------------------------------------
!
      subroutine set_rj_phys_for_pol_kene(sph_rj, rj_fld)
!
      use m_base_field_labels
      use t_spheric_rj_data
      use t_phys_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: inod, ist_fld, i, k, j
!
!
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. velocity%name) then
          ist_fld = rj_fld%istack_component(i-1)+1
!$omp parallel do private(j,inod)
          do k = 1, sph_rj%nidx_rj(1)
            do j = 1, sph_rj%nidx_rj(2)
              inod = (k-1) * sph_rj%nidx_rj(2) + j
              rj_fld%d_fld(inod,ist_fld+2) =  zero
            end do
          end do
!$omp end parallel do
        end if
      end do
!
      end subroutine set_rj_phys_for_pol_kene
!
! ----------------------------------------------------------------------
!
      subroutine set_rj_phys_for_convective_kene(sph, rj_fld)
!
      use t_spheric_parameter
      use t_phys_data
!
      use m_base_field_labels
      use m_phys_constants
      use cal_zonal_mean_sph_spectr
!
      type(sph_grids), intent(in) :: sph
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ist_fld, i
!
!
      do i = 1, rj_fld%num_phys
        if     (rj_fld%phys_name(i) .eq. velocity%name) then
          ist_fld = rj_fld%istack_component(i-1)+1
          call delete_zonal_mean_rj_field                               &
     &       (n_vector, ist_fld, sph%sph_rj, rj_fld)
        end if
      end do
!
      end subroutine set_rj_phys_for_convective_kene
!
! ----------------------------------------------------------------------
!
      subroutine cal_zm_energy_to_pressure(nidx_rtp, numnod,            &
     &          nfield_nod, ncomp_nod, istack_comp, rj_fld, d_nod)
!
      use m_base_field_labels
      use t_phys_data
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: nfield_nod, ncomp_nod
      integer (kind = kint), intent(in) :: istack_comp(0:nfield_nod)
      type(phys_data), intent(in) :: rj_fld
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
!
      integer(kind = kint) :: inod, i, k, l, m, jnod
      integer(kind = kint) :: i_velo, i_press
      real(kind = kreal) :: ave_ratio
!
!
      i_velo = 0
      i_press = 0
      do i = 1, rj_fld%num_phys
        if     (rj_fld%phys_name(i) .eq. velocity%name) then
          i_velo =  istack_comp(i- 1) + 1
        else if(rj_fld%phys_name(i) .eq. pressure%name) then
          i_press = istack_comp(i- 1) + 1
        end if
      end do
!
!$omp parallel do private(k,l,m,inod)
      do m = 1, nidx_rtp(3)
        do l = 1, nidx_rtp(2)
          do k = 1, nidx_rtp(1)
            inod = k + (l-1)*nidx_rtp(1)                            &
     &                   + (m-1)*nidx_rtp(1)*nidx_rtp(2)
            d_nod(inod,i_press) = half * (d_nod(inod,i_velo  )**2   &
     &                                  + d_nod(inod,i_velo+1)**2   &
     &                                  + d_nod(inod,i_velo+2)**2)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel
      do m = 2, nidx_rtp(3)
!$omp do private(k,l,inod,jnod)
        do l = 1, nidx_rtp(2)
          do k = 1, nidx_rtp(1)
            jnod = k + (l-1)*nidx_rtp(1)
            inod = k + (l-1)*nidx_rtp(1)                            &
     &                   + (m-1)*nidx_rtp(1)*nidx_rtp(2)
            d_nod(jnod,i_press) = d_nod(jnod,i_press)               &
     &                           + d_nod(inod,i_press)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      ave_ratio = one / dble(nidx_rtp(3))
!$omp parallel do private(k,l,inod)
      do l = 1, nidx_rtp(2)
        do k = 1, nidx_rtp(1)
          inod = k + (l-1)*nidx_rtp(1)
          d_nod(inod,i_press) = d_nod(inod,i_press) * ave_ratio
        end do
      end do
!$omp end parallel do
!
!$omp parallel
      do m = 2, nidx_rtp(3)
!$omp do private(k,l,inod,jnod)
        do l = 1, nidx_rtp(2)
          do k = 1, nidx_rtp(1)
            jnod = k + (l-1)*nidx_rtp(1)
            inod = k + (l-1)*nidx_rtp(1)                            &
     &                   + (m-1)*nidx_rtp(1)*nidx_rtp(2)
            d_nod(inod,i_press) = d_nod(jnod,i_press)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine cal_zm_energy_to_pressure
!
! ----------------------------------------------------------------------
!
     end module SPH_analyzer_zm_energies
