!analyzer_sph_neutral_point.f90
!      module analyzer_sph_neutral_point
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine analyze
!      subroutine init_analyzer
!
      module analyzer_sph_neutral_point
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_schmidt_poly_on_rtm
      use m_parallel_var_dof
      use field_IO_select
      use m_spheric_parameter
!
      implicit none
!
      real(kind = kreal) :: depth_high_t, depth_low_t
      real(kind = kreal) :: high_temp, low_temp, r_neut
      real(kind = kreal), allocatable :: freq2(:), freq(:)
!
      integer(kind = kint), parameter :: id_neutral_pt = 98
      integer(kind = kint), parameter :: id_ave_den = 99
      character(len=kchara), parameter                                  &
     &         :: fname_neutral_pt = 'neutral_point.dat'
      character(len=kchara), parameter                                  &
     &         :: fname_ave_den =    'fluxes.dat'
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_t_step_parameter
      use m_ctl_data_4_sph_utils
      use m_ctl_params_sph_utils
!      use m_read_mesh_data
      use m_node_phys_address
      use m_sph_spectr_data
      use m_sph_phys_address
      use load_mesh_data
      use const_mesh_info
      use load_data_for_sph_IO
      use copy_rj_phys_data_4_IO
      use count_num_sph_smp
      use set_phys_name_4_sph_trans
      use init_sph_trans
      use cal_rms_fields_by_sph
      use add_nodal_fields_ctl
!
      use m_ctl_data_4_fields
      use m_phys_labels
      use set_radius_func
      use set_radius_4_sph_dynamo
      use set_radius_func_noequi
!
!     --------------------- 
!
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_utils'
      call read_control_data_sph_utils
!
      call add_phys_name_tmp(fhd_grad_temp)
      visualize_ctl(num_nod_phys_ctl) =    'Viz_on'
      monitor_ctl(num_nod_phys_ctl) =      'Monitor_on'
      call add_phys_name_tmp(fhd_grad_composit)
      visualize_ctl(num_nod_phys_ctl) =    'Viz_on'
      monitor_ctl(num_nod_phys_ctl) =      'Monitor_on'
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_data_4_sph_utils'
      call set_ctl_data_4_sph_utils
!
      call time_prog_barrier
!
!       set mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      call time_prog_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
      call time_prog_barrier
!
!       set spectr grids
!
      if (iflag_debug.gt.0) write(*,*) 'input_sph_trans_grids'
      call input_sph_trans_grids(my_rank)
!
!  ------  initialize spectr data
!
      if (iflag_debug.gt.0) write(*,*) 'sel_read_alloc_step_SPH_file'
      call sel_read_alloc_step_SPH_file(my_rank, i_step_init)
!
      if (iflag_debug.gt.0) write(*,*) 'set_sph_sprctr_data_address'
      call set_sph_sprctr_data_address
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
      call copy_sph_name_rj_to_rtp
!
!  -------------------------------
!
      nlayer_ICB = 1
      nlayer_CMB = nidx_rj(1)
      depth_high_t = 7.0d0 / 13.0d0
      depth_low_t =  depth_high_t + one
      high_temp = one
      low_temp = zero
      allocate(freq2(nidx_rj(1)), freq(nidx_rj(1)))
      if (iflag_debug.gt.0) write(*,*) 'set_radius_dat_4_sph_dynamo'
      call set_radius_dat_4_sph_dynamo
      if (iflag_debug.gt.0) write(*,*) 'set_dr_for_nonequi'
      call set_dr_for_nonequi
      call time_prog_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_matrices'
      call const_2nd_fdm_matrices
      call time_prog_barrier
!
!      if (iflag_debug.gt.0) write(*,*) 's_cal_sph_bc_fdm_matrices'
!      call s_cal_sph_bc_fdm_matrices
!
      if (iflag_debug.gt.0) write(*,*) 'const_2nd_fdm_coefs'
      call const_2nd_fdm_coefs
      call time_prog_barrier
!
! ---------------------------------
!
      call allocate_phys_rj_data
      call init_rms_4_sph_spectr
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_trans'
      call initialize_sph_trans
!
      call time_prog_barrier
!      call check_schmidt_poly_rtm(my_rank+40)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_t_step_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_ctl_params_sph_utils
      use m_sph_spectr_data
      use m_sph_phys_address
      use copy_rj_phys_data_4_IO
!
      integer(kind = kint) :: i_step, k, inod
      real(kind = kreal) :: real_grad_t0, real_comp_t0
!
!
      if(idx_rj_degree_zero .gt. 0) then
        open(id_neutral_pt,file=fname_neutral_pt)
        open(id_ave_den,file=fname_ave_den)
        write(id_neutral_pt,'(a)') 'step, neutral_point'
        write(id_ave_den,'(a)')                                         &
     &      'step, radial_id, heat_flux, comp_flux, density_flux, freq'
      end if
!
      do i_step = i_step_init, i_step_number, i_step_output_ucd
!
!   Input spectr data
!
        if(idx_rj_degree_zero .gt. 0) then
          if(iflag_debug.gt.0)                                          &
     &                write(*,*) 'sel_read_step_SPH_field_file'
          call sel_read_step_SPH_field_file(my_rank, i_step)
!
!          if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
          call set_rj_phys_data_from_IO
        end if
        call time_prog_barrier
!
!  pickup components
!
        if(idx_rj_degree_zero .gt. 0) then
!          if (iflag_debug.gt.0) write(*,*) 'set_radial_grad_scalars'
          call set_radial_grad_scalars(i_step)
        end if
!
        if(idx_rj_degree_zero .gt. 0) then
          do k = nlayer_ICB, nlayer_CMB
            inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
            real_grad_t0 = d_rj(inod,ipol%i_grad_t)                     &
     &                    * ar_1d_rj(k,2)
            real_comp_t0 = d_rj(inod,ipol%i_grad_composit)              &
     &                    * ar_1d_rj(k,2)
            write(id_ave_den,'(2i10, 1p4E25.15e3)')                     &
     &           i_step, k, real_grad_t0, real_comp_t0,                 &
     &           freq2(k), freq(k)
          end do
        end if
!
      end do
!
      if(idx_rj_degree_zero .gt. 0) then
        close(id_neutral_pt)
        close(id_ave_den)
      end if
      if(iflag_debug.eq.1) write(*,*) 'exit analyze'
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      subroutine set_radial_grad_scalars(i_step)
!
      use cal_sph_exp_1st_diff
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_ctl_params_sph_utils
!
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint) :: k, inod
!
!
!      write(*,*) 'nlayer_ICB, nlayer_CMB', nlayer_ICB, nlayer_CMB
!      write(*,*) 'ipol%i_temp, ipol%i_grad_t',                         &
!     &          ipol%i_temp, ipol%i_grad_t
!      write(*,*) 'ipol%i_light, ipol%i_grad_composit',                 &
!     &          ipol%i_light, ipol%i_grad_composit
!
      call cal_sph_nod_gradient_2(nlayer_ICB, nlayer_CMB,               &
     &    d_rj(1,ipol%i_temp), d_rj(1,ipol%i_grad_t) )
      call cal_sph_nod_gradient_2(nlayer_ICB, nlayer_CMB,               &
     &    d_rj(1,ipol%i_light), d_rj(1,ipol%i_grad_composit) )
!
!      if(idx_rj_degree_zero .gt. 0) then
!
        do k = nlayer_ICB, nlayer_CMB
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          freq2(k) = (buo_ratio * d_rj(inod, ipol%i_grad_composit)      &
     &              + d_rj(inod, ipol%i_grad_t)) * ar_1d_rj(k,2)
          if(freq2(k) .gt. 0.0d0) freq(k) = sqrt(freq2(k))
          freq2(k) = freq2(k) * radius_1d_rj_r(k  )**2
        end do
        do k = nlayer_CMB-2, nlayer_ICB+1, - 1
          if( freq2(k).lt.0.0d0 .and. freq2(k+1).ge.0.0d0) then
            r_neut = (radius_1d_rj_r(k  )*abs(freq2(k+1))               &
     &             + radius_1d_rj_r(k+1)*abs(freq2(k)  ) )              &
     &             / (abs(freq2(k+1) - freq2(k)))
            write(id_neutral_pt,*) i_step, r_neut
          end if
        end do
!
!      end if
!
       end subroutine set_radial_grad_scalars
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_neutral_point
