!SPH_analyzer_zm_energies.f90
!     module SPH_analyzer_zm_energies
!
!      Written by H. Matsui
!
!      subroutine SPH_analyze_zm_energies(i_step, visval)
!
      module SPH_analyzer_zm_energies
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
      private :: cal_zm_energy_to_pressure
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zm_energies(i_step, visval, fld_IO)
!
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_control_params_2nd_files
      use m_node_id_spherical_IO
      use t_field_data_IO
!
      use field_IO_select
      use r_interpolate_sph_data
      use copy_rj_phys_data_4_IO
!
      use sph_transfer_all_field
      use cal_zonal_mean_sph_spectr
      use set_exit_flag_4_visualizer
!
!
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(inout) :: visval
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: i_udt
!
!
      call set_output_flag(i_udt, i_step, i_step_output_ucd)
      call set_output_flag_4_viz(i_step, visval)
      visval = visval * i_udt
!
      if(visval .eq. 0) then
!
!   Input spectr data
        if (iflag_debug.gt.0) write(*,*) 'sel_read_step_SPH_field_file'
      call  sel_read_step_SPH_field_file                                &
     &     (nprocs, my_rank, i_step, fld_IO)
!
!    copy and extend magnetic field to outside
!
        if(iflag_org_sph_rj_head .eq. 0) then
          if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
          call set_rj_phys_data_from_IO(fld_IO)
        else
          if (iflag_debug.gt.0) write(*,*)                              &
     &                        'r_interpolate_sph_fld_from_IO'
          call r_interpolate_sph_fld_from_IO(fld_IO)
        end if
!
!        call set_rj_phys_for_pol_kene
        call set_rj_phys_for_convective_kene
!
!  spherical transform for vector
        call sph_b_trans_all_field
        call cal_zm_energy_to_pressure
!
      end if
!
      end subroutine SPH_analyze_zm_energies
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_energies
!
      use m_ctl_data_4_fields
      use m_phys_labels
!
      integer(kind = kint) :: ifld, iflag_velo
!
      iflag_velo =  0
      do ifld = 1, field_ctl%num
        if(field_ctl%c1_tbl(ifld) .eq. fhd_velo)  iflag_velo =  2
      end do
!
      call deallocate_phys_control
!
      field_ctl%num = iflag_velo
      call alloc_control_array_c3(field_ctl)
!
      ifld = 0
      if(iflag_velo .gt. 0) then
        ifld = ifld+1
        field_ctl%c1_tbl(ifld) = fhd_velo
        field_ctl%c2_tbl(ifld) = 'Viz_On'
        field_ctl%c3_tbl(ifld) = 'Monitor_Off'
!
        ifld = ifld+1
        field_ctl%c1_tbl(ifld) = fhd_press
        field_ctl%c2_tbl(ifld) = 'Viz_On'
        field_ctl%c3_tbl(ifld) = 'Monitor_Off'
      end if
!
      end subroutine set_ctl_data_4_zm_energies
!
! ----------------------------------------------------------------------
!
      subroutine set_rj_phys_for_pol_kene
!
      use m_phys_labels
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
!
      integer(kind = kint) :: inod, ist_fld, i, k, j
!
!
      do i = 1, num_phys_rj
        if     (phys_name_rj(i) .eq. fhd_velo) then
          ist_fld = istack_phys_comp_rj(i-1)+1
!$omp parallel do private(j,inod)
          do k = 1, nidx_rj(1)
            do j = 1, nidx_rj(2)
              inod = (k-1)*nidx_rj(2) + j
              d_rj(inod,ist_fld+2) =  zero
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
      subroutine set_rj_phys_for_convective_kene
!
      use m_phys_labels
      use m_sph_spectr_data
      use m_phys_constants
      use m_sph_phys_address
      use cal_zonal_mean_sph_spectr
!
!
      integer(kind = kint) :: ist_fld, i
!
!
      do i = 1, num_phys_rj
        if     (phys_name_rj(i) .eq. fhd_velo) then
          ist_fld = istack_phys_comp_rj(i-1)+1
          call delete_zonal_mean_rj_field(n_vector, ist_fld)
        end if
      end do
!
      end subroutine set_rj_phys_for_convective_kene
!
! ----------------------------------------------------------------------
!
      subroutine cal_zm_energy_to_pressure
!
      use m_phys_labels
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_node_phys_data
!
!
      integer(kind = kint) :: inod, i, k, l, m, jnod
      integer(kind = kint) :: i_velo, i_press
      real(kind = kreal) :: ave_ratio
!
!
      do i = 1, num_phys_rj
        if     (phys_name_rj(i) .eq. fhd_velo) then
          i_velo =  nod_fld1%istack_component(i- 1) + 1
        else if(phys_name_rj(i) .eq. fhd_press) then
          i_press = nod_fld1%istack_component(i- 1) + 1
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
