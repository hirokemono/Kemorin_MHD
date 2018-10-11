!>@file   analyzer_update_sph_rst.f90
!!@brief  module analyzer_update_sph_rst
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_update_sph_rst
!!      subroutine analyze_update_sph_rst
!!@endverbatim
!
      module analyzer_update_sph_rst
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_control_param_newsph
      use r_interpolate_marged_sph
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_assembled_field_IO
      use t_control_data_4_merge
!
      use new_SPH_restart
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
      use assemble_sph_fields
!
      implicit none
!
      type(control_data_4_merge), save :: mgd_ctl_s
      type(time_data), save :: init_t
!
      type(sph_mesh_data), allocatable, save :: org_sph_mesh(:)
      type(phys_data), allocatable, save ::     org_sph_phys(:)
!
      type(sph_mesh_data), allocatable, save :: new_sph_mesh(:)
      type(phys_data), allocatable, save ::     new_sph_phys(:)
!
      integer(kind = kint) :: nloop_new
      type(field_IO), allocatable, save :: new_fst_IO(:)
      type(time_data), save :: fst_time_IO
!
      type(sph_radial_itp_data), save :: r_itp
      type(rj_assemble_tbl), allocatable, save :: j_table(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_update_sph_rst
!
      use m_error_IDs
!
      use bcast_4_assemble_sph_ctl
      use sph_file_MPI_IO_select
      use sph_file_IO_select
      use field_IO_select
      use share_spectr_index_data
      use count_nnod_4_asseble_sph
!
      integer(kind = kint) :: ip, jp
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      if(my_rank .eq. 0) call read_control_assemble_sph(mgd_ctl_s)
      call bcast_merge_control_data(mgd_ctl_s)
      call set_control_4_newsph(mgd_ctl_s)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           istep_start, istep_end, increment_step
!
      allocate( org_sph_mesh(np_sph_org) )
      allocate( org_sph_phys(np_sph_org) )
      allocate( new_sph_mesh(np_sph_new) )
      allocate( new_sph_phys(np_sph_new) )
      allocate(j_table(np_sph_org,np_sph_new))
!
!  set original spectr data
!
      iflag_sph_file_fmt = ifmt_org_sph_file
      call set_local_rj_mesh_4_merge                                    &
     &   (org_sph_head, np_sph_org, org_sph_mesh)
      call share_org_sph_rj_data(np_sph_org, org_sph_mesh)
!
!  set new spectr data
!
      iflag_sph_file_fmt = ifmt_new_sph_file
      call set_local_rj_mesh_4_merge                                    &
     &   (new_sph_head, np_sph_new, new_sph_mesh)
      call load_new_spectr_rj_data(np_sph_org, np_sph_new,              &
     &    org_sph_mesh, new_sph_mesh, j_table)
!
!     Share number of nodes for new mesh
!
      nloop_new = (np_sph_new-1)/nprocs + 1
      allocate(new_fst_IO(nloop_new))
!
      call s_count_nnod_4_asseble_sph                                   &
     &   (np_sph_new, new_sph_mesh, nloop_new, new_fst_IO)
!
!     construct radial interpolation table
!
      call const_r_interpolate_table(np_sph_org, np_sph_new,            &
     &    org_sph_mesh, new_sph_mesh, r_itp)
!
!      Construct field list from spectr file
!
      call load_field_name_assemble_sph                                 &
     &   (istep_start, np_sph_org, org_sph_fst_param,                   &
     &    org_sph_phys(1), new_sph_phys(1), fst_time_IO)
!
      call share_spectr_field_names(np_sph_org, np_sph_new,             &
     &    new_sph_mesh, org_sph_phys, new_sph_phys)
!
!
!      do jp = 1, np_sph_new
!        if(mod(jp-1,nprocs) .ne. my_rank) cycle
!        do ip = 1, np_sph_org
!          do j = 1, org_sph_mesh(1)%sph%sph_rj%nidx_rj(2)
!            if(j_table(ip,jp)%j_org_to_new(j).gt. 0)                   &
!     &          write(50+my_rank,*) jp, ip, j,                         &
!     &                              j_table(ip,jp)%j_org_to_new(j)
!          end do
!        end do
!      end do
!      write(*,*) 'init_update_sph_rst end'
!
      end subroutine init_update_sph_rst
!
! ----------------------------------------------------------------------
!
      subroutine analyze_update_sph_rst
!
      use m_phys_labels
      use r_interpolate_marged_sph
      use set_field_file_names
      use share_field_data
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: ip, jp, irank_new
      integer(kind = kint) :: iloop, jloop
      integer(kind = kint) :: istep_out
!
!
!     ---------------------
!
      do istep = istep_start, istep_end, increment_step
!
!     Load original spectr data
        do iloop = 0, (np_sph_org-1)/nprocs
          irank_new = my_rank + iloop * nprocs
          ip = irank_new + 1
          call load_old_fmt_sph_data                                    &
     &       (irank_new, istep, np_sph_org, org_sph_fst_param,          &
     &        org_sph_mesh(ip)%sph, org_sph_phys(ip))
          call calypso_mpi_barrier
        end do
!
        istep_out = istep
        if(iflag_newtime .gt. 0) then
          istep_out =          istep_new_rst / increment_new_step
          init_t%i_time_step = istep_new_rst
          init_t%time =        time_new
        else
          call reset_time_data(init_t)
        end if
!
        call share_time_step_data(init_t)
!
!     Bloadcast original spectr data
        do ip = 1, np_sph_org
          call share_each_field_data(ip, org_sph_phys(ip))
!
!     Copy spectr data to temporal array
          do jp = 1, np_sph_new
           if(mod(jp-1,nprocs) .ne. my_rank) cycle
            call set_assembled_sph_data(org_sph_mesh(ip)%sph,           &
     &          new_sph_mesh(jp)%sph, j_table(ip,jp), r_itp,            &
     &          org_sph_phys(ip), new_sph_phys(jp))
          end do
          call dealloc_phys_data_type(org_sph_phys(ip))
        end do
!
        do jloop = 1, nloop_new
          irank_new = my_rank + (jloop-1) * nprocs
          jp = irank_new + 1

          if(irank_new .lt. np_sph_new) then
            call const_assembled_sph_data                               &
     &          (b_sph_ratio, init_t, new_sph_mesh(jp)%sph, r_itp,      &
     &           new_sph_phys(jp), new_fst_IO(jloop), fst_time_IO)
          end if
        end do
!
        call sel_write_SPH_assemble_field(np_sph_new, istep_out,        &
     &      nloop_new, new_sph_fst_param, fst_time_IO, new_fst_IO)
!
        do jloop = 1, nloop_new
          irank_new = my_rank + (jloop-1) * nprocs
          if(irank_new .lt. np_sph_new) then
            call dealloc_phys_data_IO(new_fst_IO(jloop))
            call dealloc_phys_name_IO(new_fst_IO(jloop))
          end if
        end do
        call calypso_mpi_barrier
      end do
!
      do jp = 1, np_sph_new
        if(mod(jp-1,nprocs) .ne. my_rank) cycle
        do ip = 1, np_sph_org
          call dealloc_mode_table_4_assemble(j_table(ip,jp))
        end do
      end do
      deallocate(j_table)
!
      deallocate(org_sph_mesh, org_sph_phys)
      deallocate(new_sph_mesh, new_sph_phys)
!
      call calypso_MPI_barrier
!
      if(iflag_delete_org_sph .gt. 0) then
        icou = 0
        do istep = istep_start, istep_end, increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_SPH_fld_file                                      &
     &        (org_sph_fst_param, np_sph_org, istep)
        end do
      end if
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_update_sph_rst
!
! ----------------------------------------------------------------------
!
      end module analyzer_update_sph_rst
