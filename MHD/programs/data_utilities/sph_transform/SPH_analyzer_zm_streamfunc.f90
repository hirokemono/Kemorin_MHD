!SPH_analyzer_zm_streamfunc.f90
!     module SPH_analyzer_zm_streamfunc
!
!      Written by H. Matsui
!
!      subroutine SPH_analyze_zm_streamfunc(i_step, visval)
!
      module SPH_analyzer_zm_streamfunc
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
      private :: set_rj_phys_for_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_zm_streamfunc(i_step, visval)
!
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_field_data_IO
      use m_node_id_spherical_IO
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
        call sel_read_step_SPH_field_file(my_rank, i_step)
!
!    copy and extend magnetic field to outside
!
        if(iflag_org_sph_rj_head .eq. 0) then
          if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
          call set_rj_phys_data_from_IO
        else
          if (iflag_debug.gt.0) write(*,*)                              &
     &                        'r_interpolate_sph_fld_from_IO'
          call r_interpolate_sph_fld_from_IO
        end if
!
        call set_rj_phys_for_zm_streamfunc
        call zonal_mean_all_sph_spectr
!
!  spherical transform for vector
        call sph_b_trans_streamline
!
      end if
!
      end subroutine SPH_analyze_zm_streamfunc
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_data_4_zm_streamline
!
      use m_ctl_data_4_fields
      use m_phys_labels
!
      integer(kind = kint) :: ifld, iflag_velo, iflag_magne
!
      iflag_velo =  0
      iflag_magne = 0
      do ifld = 1, num_nod_phys_ctl
        if(phys_nod_name_ctl(ifld) .eq. fhd_velo)  iflag_velo =  2
        if(phys_nod_name_ctl(ifld) .eq. fhd_magne) iflag_magne = 2
      end do
!
      call deallocate_phys_control
!
      num_nod_phys_ctl = iflag_velo + iflag_magne
      call allocate_phys_control
!
      ifld = 0
      if(iflag_velo .gt. 0) then
        phys_nod_name_ctl(ifld+1) = fhd_velo
        phys_nod_name_ctl(ifld+2) = fhd_vort
        ifld = ifld+2
      end if
      if(iflag_magne .gt. 0) then
        phys_nod_name_ctl(ifld+1) = fhd_magne
        phys_nod_name_ctl(ifld+2) = fhd_current
        ifld = ifld+2
      end if
!
      do ifld = 1, num_nod_phys_ctl
        visualize_ctl(ifld) =  'Viz_On'
        monitor_ctl(ifld) =    'Monitor_Off'
      end do
!
      end subroutine set_ctl_data_4_zm_streamline
!
! ----------------------------------------------------------------------
!
      subroutine sph_b_trans_streamline
!
      use sph_trans_vector
      use copy_all_spec_4_sph_trans
      use copy_all_field_4_sph_trans
      use pole_sph_transform
!
!
      if (num_vector_rtp .gt. 0) then
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'set_all_vec_spec_to_sph_t'
        call set_all_vec_spec_to_sph_t
!
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'sph_b_trans_vector', num_vector_rtp
        call sph_b_trans_vector(num_vector_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &      write(*,*) 'pole_b_trans_vector'
        call pole_b_trans_vector(num_vector_rtp)
!
        if (iflag_debug.gt.0)                                           &
     &        write(*,*) 'set_xyz_vect_from_sph_trans'
        call adjust_phi_comp_for_streamfunc
        call set_xyz_vect_from_sph_trans
      end if
!
      end subroutine sph_b_trans_streamline
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_phys_for_zm_streamfunc
!
      use m_phys_labels
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      integer(kind = kint) :: inod, k, j
!
!
      if(ipol%i_velo .gt. 0) then
!$omp parallel do private(j,inod)
        do k = 1, nidx_rj(1)
          do j = 1, nidx_rj(2)
            inod = (k-1)*nidx_rj(2) + j
            d_rj(inod,itor%i_vort ) =  d_rj(inod,itor%i_velo)
            d_rj(inod,ipol%i_vort ) =  zero
            d_rj(inod,idpdr%i_vort) =  zero
!
            d_rj(inod,itor%i_velo ) =  d_rj(inod,ipol%i_velo)
            d_rj(inod,ipol%i_velo ) =  zero
            d_rj(inod,idpdr%i_velo) =  zero
           end do
        end do
!$omp end parallel do
      end if
!
      if(ipol%i_magne .gt. 0) then
!$omp parallel do private(j,inod)
        do k = 1, nidx_rj(1)
          do j = 1, nidx_rj(2)
            inod = (k-1)*nidx_rj(2) + j
            d_rj(inod,itor%i_current ) =  d_rj(inod,itor%i_magne)
            d_rj(inod,ipol%i_current ) =  zero
            d_rj(inod,idpdr%i_current) =  zero
!
            d_rj(inod,itor%i_magne ) =  d_rj(inod,ipol%i_magne)
            d_rj(inod,ipol%i_magne ) =  zero
            d_rj(inod,idpdr%i_magne) =  zero
           end do
        end do
!$omp end parallel do
      end if
!
      end subroutine set_rj_phys_for_zm_streamfunc
!
! ----------------------------------------------------------------------
!
      subroutine adjust_phi_comp_for_streamfunc
!
      use m_phys_labels
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_work_4_sph_trans
!
!
      integer(kind = kint) :: inod, j, k, l, m, jnod
!
!
      do j = 1, num_vector_rtp
!
!$omp parallel do private(k,l,m,inod,jnod)
          do m = 1, nidx_rtp(3)
            do l = 1, nidx_rtp(2)
              do k = 1, nidx_rtp(1)
                inod = k + (l-1)*nidx_rtp(1)                            &
     &                   + (m-1)*nidx_rtp(1)*nidx_rtp(2)
                jnod = j + (inod-1)*num_vector_rtp
                vr_rtp(3*jnod-2) =  zero
                vr_rtp(3*jnod-1) =  zero
                vr_rtp(3*jnod  ) = -vr_rtp(3*jnod  )                    &
     &                      * radius_1d_rtp_r(k)*sin_theta_1d_rtp(l)
              end do
            end do
          end do
!$omp end parallel do
      end do
!
      end subroutine adjust_phi_comp_for_streamfunc
!
! ----------------------------------------------------------------------
!
     end module SPH_analyzer_zm_streamfunc
