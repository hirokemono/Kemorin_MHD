!analyzer_ene_zonal_spec.f90
!      module analyzer_ene_zonal_spec
!..................................................
!
!      modified by H. Matsui on Nov., 2007 
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_ene_zonal_spec
!
      use m_precision
      use m_machine_parameter
      use field_IO_select
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
      use calypso_mpi
      use m_ctl_data_4_zonal_fft
      use m_ctl_params_zonal_fft
      use m_node_id_spherical_IO
      use sph_file_IO_select
      use copy_sph_node_4_IO
      use count_num_sph_smp
      use copy_rtp_phys_data_4_IO
      use m_sph_zonal_ene_spectr
      use cal_zonal_ene_spec
!
!     --------------------- 
!
      call read_control_data_zonal_fft
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_zonal_fft'
      call s_set_ctl_data_4_zonal_fft
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
!     --------------------- 
!
      call sel_read_alloc_step_FEM_file(my_rank, istep_start)
!
      call copy_rtp_phys_name_from_IO
      call copy_rtp_phys_data_from_IO
!
!     --------------------- 
!
      call set_range_4_zonal_ene_spec
!
      call allocate_zonal_ene_spec_idx
      call allocate_zonal_ene_spec_data
!
      call set_idx_4_zonal_ene_spec
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use calypso_mpi
      use m_ctl_params_zonal_fft
      use m_sph_zonal_ene_spectr
      use copy_rtp_phys_data_4_IO
      use cal_zonal_ene_spec
!
      integer(kind = kint) :: istep, icou_time
!
!
      icou_time = 0
      do istep = istep_start, istep_end, istep_int
        call sel_read_step_FEM_field_file(my_rank, istep)
        call copy_rtp_phys_data_from_IO
!
        call s_cal_zonal_ene_spec(icou_time)
        call write_zonal_ene_spec(my_rank, istep)
      end do
!
      call cal_tave_zonal_ene_spec(icou_time)
!
      call write_tave_zonal_ene_spec(my_rank)
      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_ene_zonal_spec
