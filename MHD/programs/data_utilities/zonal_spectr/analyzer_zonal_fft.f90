!analyzer_zonal_fft.f90
!
!      module analyzer_zonal_fft
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
!
!..................................................
!
      module analyzer_zonal_fft
!
      use m_precision
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use m_ctl_data_4_zonal_fft
      use m_ctl_params_zonal_fft
      use m_read_mesh_data
      use m_node_id_spherical_IO
      use m_ucd_input_data
      use load_mesh_data
      use const_mesh_info
      use sph_file_IO_select
      use copy_sph_node_4_IO
      use transfer_field_2_rtp_grid
      use copy_rtp_phys_data_4_IO
      use count_num_sph_smp
!
!     ---------------------
!
      call read_control_data_zonal_fft
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_zonal_fft'
      call s_set_ctl_data_4_zonal_fft
!
!       set mesh informations
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
!       set spectr grids
!
      call sel_read_geom_rtp_file(my_rank)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_sph_node_rtp_from_IO'
      call copy_sph_node_rtp_from_IO
!
      if (iflag_debug.gt.0) write(*,*) 's_count_num_sph_smp'
      call s_count_num_sph_smp
!
!  -------------------------------
!
      call allocate_phys_data_by_output(my_rank, istep_start)
!
!
      if (iflag_debug.gt.0) write(*,*) 'copy_phys_name_to_rtp_phys'
      call copy_phys_name_to_rtp_phys
      call copy_rtp_phys_name_to_IO
!
      call allocate_work_4_vect_trans
!
       end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_ctl_params_zonal_fft
      use m_sph_spectr_data
      use m_ucd_input_data
      use transfer_field_2_rtp_grid
      use field_IO_select
      use copy_rtp_phys_data_4_IO
      use FFT_selector
!
      integer(kind = kint) :: istep
      integer(kind = kint) :: ncomp
      integer(kind = kint), allocatable :: Nstacksmp(:)
!
!
      allocate( Nstacksmp(0:np_smp) )
      Nstacksmp = -1
!
      ncomp = ntot_phys_rtp*nidx_rtp(1)*nidx_rtp(2)
      Nstacksmp(0:np_smp) = ntot_phys_rtp*irt_rtp_smp_stack(0:np_smp)
!
      call initialize_FFT_select(my_rank, np_smp, Nstacksmp,            &
     &    nidx_rtp(3))
!
      do istep = istep_start, istep_end, istep_int
        call set_data_by_read_ucd(my_rank, istep)
!
        if (iflag_debug.gt.0) write(*,*) 'trans_nod_phys_2_rtp_phys'
        call trans_nod_phys_2_rtp_phys(ifrag_trans_vect)
!
        call forward_FFT_select(np_smp, Nstacksmp, ncomp,               &
     &      nidx_rtp(3), d_zonal(1,1) )
!
!        do i = 1, nnod_rtp
!          write(60,*) i, d_zonal(1:ntot_phys_rtp,i)
!        end do
!
        call copy_zonal_phys_to_rtp
        call copy_rtp_phys_data_to_IO
!
        call sel_write_step_FEM_field_file(my_rank, istep)
      end do
!
      deallocate( Nstacksmp )
!
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_zonal_fft
