!>@file   analyzer_para_assemble_sph.f90
!!@brief  module analyzer_para_assemble_sph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine init_para_assemble_sph
!!      subroutine analyze_para_assemble_sph
!!@endverbatim
!
      module analyzer_para_assemble_sph
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_merge_spheric_mesh
      use m_control_param_newsph
      use parallel_assemble_sph
      use copy_rj_phys_type_4_IO
      use r_interpolate_marged_sph
!
      implicit none
!
      type(sph_mesh_data), allocatable, save :: org_sph_mesh(:)
      type(phys_data), allocatable, save ::     org_sph_phys(:)
!
      type(sph_mesh_data), allocatable, save :: new_sph_mesh(:)
      type(phys_data), allocatable, save ::     new_sph_phys(:)
!
!
      type(sph_radial_itp_data), save :: r_itp
      type(rj_assemble_tbl), allocatable, save :: j_table(:)
      integer(kind = kint) :: nlayer_ICB_org, nlayer_CMB_org
      integer(kind = kint) :: nlayer_ICB_new, nlayer_CMB_new
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_para_assemble_sph
!
      use m_control_data_4_merge
!
      use m_node_id_spherical_IO
      use field_IO_select
!
      integer(kind = kint) :: ip, irank_org, j
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      call read_control_assemble_sph
      call set_control_4_newsph
!
      if(nprocs .ne. np_sph_new) then
        call calypso_MPI_abort(1,                                       &
     &      'num. of process has to be the number of target processes')
      end if
!
      allocate( org_sph_mesh(np_sph_org) )
      allocate( org_sph_phys(np_sph_org) )
      allocate( new_sph_mesh(1) )
      allocate( new_sph_phys(1) )
!
!  set original spectr data
!
      iflag_sph_file_fmt = ifmt_org_sph_file
      do ip = 1, np_sph_org
        irank_org = ip - 1
        if(mod(irank_org,np_sph_new) .ne. my_rank) cycle
!
        call set_local_rj_mesh_4_merge(irank_org, org_sph_mesh(ip))
        call set_sph_boundary_4_merge(org_sph_mesh(ip)%sph_grps,        &
     &    nlayer_ICB_org, nlayer_CMB_org)
      end do
!
!
      do ip = 1, np_sph_org
        irank_org = mod(ip - 1,np_sph_new)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%sph_rank_rj,    &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_global_rj, &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,        &
     &      ione, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj,        &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%ist_rj,         &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%ied_rj,         &
     &      itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      end do
!
      do ip = 1, np_sph_org
        if(mod(ip-1,np_sph_new) .eq. my_rank) cycle
!
        call alloc_type_spheric_param_rj                                &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj)
        call alloc_type_sph_1d_index_rj                                 &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj)
      end do
!
      do ip = 1, np_sph_org
        irank_org = mod(ip - 1,np_sph_new)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%inod_global_rj, &
     &      org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,                   &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%idx_global_rj,  &
     &      itwo*org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,              &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%radius_1d_rj_r, &
     &      org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(1),                &
     &      CALYPSO_REAL, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_r, &
     &      org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(1),                &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_j, &
     &      ithree*org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2),         &
     &      CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
      end do
!
!  set new spectr data
!
      iflag_sph_file_fmt = ifmt_new_sph_file
      sph_file_head = new_sph_head
      call set_local_rj_mesh_4_merge(my_rank, new_sph_mesh(1))
      call set_sph_boundary_4_merge(new_sph_mesh(1)%sph_grps,           &
     &    nlayer_ICB_new, nlayer_CMB_new)
      write(*,*) 'nlayer_ICB_org: ', nlayer_ICB_org, nlayer_CMB_org
      write(*,*) 'nlayer_ICB_new: ', nlayer_ICB_new, nlayer_CMB_new
!
!     Construct mode transfer table
!
      allocate(j_table(np_sph_org))
      do ip = 1, np_sph_org
        call alloc_each_mode_tbl_4_assemble                             &
     &      (org_sph_mesh(ip)%sph_mesh, j_table(ip))
        call set_mode_table_4_assemble(org_sph_mesh(ip)%sph_mesh,       &
     &      new_sph_mesh(1)%sph_mesh, j_table(ip))
      end do
!
!     construct interpolation table
!
      if(my_rank .eq. 0) then
        call sph_radial_interpolation_coef                              &
     &     (org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      org_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r,             &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r, r_itp)
       end if
!
      call MPI_Bcast(r_itp%iflag_same_rgrid, ione, CALYPSO_INTEGER,     &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      if(r_itp%iflag_same_rgrid .eq. 0) then
        if(my_rank .ne. 0)  call allocate_radial_itp_tbl                &
     &             (new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1), r_itp)
!
        call MPI_Bcast(r_itp%kr_inner_domain, ione, CALYPSO_INTEGER,    &
     &      izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%kr_outer_domain, ione, CALYPSO_INTEGER,    &
     &      izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%k_old2new_in, r_itp%nri_old2new,           &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%k_old2new_out, r_itp%nri_old2new,          &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%coef_old2new_in, r_itp%nri_old2new,        &
     &      CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      end if
!
!      Construct field list from spectr file
!
      if(my_rank .eq. 0) then
        call load_field_name_assemble_sph                               &
     &     (istep_start, org_sph_fst_head, np_sph_org,                  &
     &      new_sph_mesh(1)%sph_mesh, org_sph_phys(1), new_sph_phys(1))
      end if
!
      do ip = 1, np_sph_org
        call MPI_Bcast(org_sph_phys(ip)%num_phys, ione,                 &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_phys(ip)%ntot_phys, ione,                &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
        if(my_rank .ne. 0) call alloc_phys_name_type(org_sph_phys(ip))
!
        call MPI_Bcast(org_sph_phys(ip)%num_component,                  &
     &      org_sph_phys(ip)%num_phys, CALYPSO_INTEGER,                 &
     &      izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_phys(ip)%istack_component,               &
     &      (org_sph_phys(ip)%num_phys+1),                              &
     &      CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_phys(ip)%iflag_monitor,                  &
     &      org_sph_phys(ip)%num_phys, CALYPSO_INTEGER,                 &
     &      izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(org_sph_phys(ip)%phys_name,                      &
     &     (org_sph_phys(ip)%num_phys*kchara),                          &
     &      CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      end do
!
!
      call MPI_Bcast(new_sph_phys(1)%num_phys, ione, CALYPSO_INTEGER,   &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_phys(1)%ntot_phys, ione, CALYPSO_INTEGER,  &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) then
        call alloc_phys_name_type(new_sph_phys(1))
        call alloc_phys_data_type                                       &
     &     (new_sph_mesh(1)%sph_mesh%sph_rj%nnod_rj, new_sph_phys(1))
      end if
!
      call MPI_Bcast(new_sph_phys(1)%num_component,                     &
     &    new_sph_phys(1)%num_phys, CALYPSO_INTEGER,                    &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_phys(1)%istack_component,                  &
     &    (new_sph_phys(1)%num_phys+1),                                 &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_phys(1)%iflag_monitor,                     &
     &    new_sph_phys(1)%num_phys, CALYPSO_INTEGER,                    &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_phys(1)%phys_name,                         &
     &   (new_sph_phys(1)%num_phys*kchara),                             &
     &    CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
!      do ip = 1, np_sph_org
!        do j = 1, org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(2)
!          if(j_table(ip)%j_org_to_new(j).gt. 0)                        &
!     &        write(50+my_rank,*) ip, j, j_table(ip)%j_org_to_new(j)
!        end do
!      end do
!
      end subroutine init_para_assemble_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_para_assemble_sph
!
      use m_phys_labels
      use m_sph_spectr_data
      use m_field_data_IO
      use m_t_step_parameter
      use field_IO_select
      use r_interpolate_marged_sph
      use copy_time_steps_4_restart
      use set_field_file_names
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: ip, irank_org, num, j
!
!
!     ---------------------
!
      do istep = istep_start, istep_end, increment_step
!
!     Load original spectr data
        phys_file_head = org_sph_fst_head
        do ip = 1, np_sph_org
          irank_org = ip - 1
          if(mod(irank_org,np_sph_new) .ne. my_rank) cycle
!
          call sel_read_alloc_step_SPH_file(irank_org, istep)
!
          call copy_time_steps_from_restart
          call alloc_phys_data_type                                     &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj, org_sph_phys(ip))
          call copy_rj_phys_type_from_IO                                &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj, org_sph_phys(ip))
!
          call deallocate_phys_data_IO
          call deallocate_phys_data_name_IO
        end do
        time = time_init
        i_step_MHD = i_step_init
!
        call MPI_Bcast(i_step_init, ione, CALYPSO_INTEGER,              &
     &        izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(time_init, ione, CALYPSO_REAL,                   &
     &        izero, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(dt, ione, CALYPSO_REAL,                          &
     &        izero, CALYPSO_COMM, ierr_MPI)
!
!     Bloadcast original spectr data
        do ip = 1, np_sph_org
          irank_org = mod(ip - 1,np_sph_new)
!
          if(mod(irank_org,np_sph_new) .ne. my_rank) then
            call alloc_phys_data_type                                   &
     &         (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,               &
     &          org_sph_phys(ip))
          end if
!
          num = org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj                &
     &         * new_sph_phys(1)%ntot_phys
          call MPI_Bcast(org_sph_phys(ip)%d_fld, num, CALYPSO_REAL,     &
     &        irank_org, CALYPSO_COMM, ierr_MPI)
!
!     Copy spectr data to temporal array
          call set_assembled_sph_data(org_sph_mesh(ip)%sph_mesh,        &
     &          new_sph_mesh(1)%sph_mesh, j_table(ip), r_itp,           &
     &          org_sph_phys(ip), new_sph_phys(1))
          call dealloc_phys_data_type(org_sph_phys(ip))
        end do
!
        call const_assembled_sph_data(my_rank, istep,                   &
     &        new_sph_mesh(1)%sph_mesh, r_itp, new_sph_phys(1))
      end do
!
      do ip = 1, np_sph_org
        call dealloc_mode_table_4_assemble(j_table(ip))
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
        phys_file_head = org_sph_fst_head
        do istep = istep_start, istep_end, increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_SPH_fld_file(iflag_field_data_fmt,                &
     &        np_sph_org, istep)
        end do
      end if
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_para_assemble_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_para_assemble_sph
