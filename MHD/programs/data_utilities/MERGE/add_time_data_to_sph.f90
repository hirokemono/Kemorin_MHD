!>@file   add_time_step_to_sph.f90
!!@brief  program add_time_step_to_sph
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
      program add_time_step_to_sph
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_field_data_IO
!
      use m_machine_parameter
      use m_control_param_newsph
      use new_SPH_restart
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
!
      use m_control_data_4_merge
!
      use m_node_id_spherical_IO
      use input_old_file_sel_4_zlib
!
      use m_sph_spectr_data
      use m_t_step_parameter
      use m_t_int_parameter
      use field_IO_select
      use r_interpolate_marged_sph
      use copy_time_steps_4_restart
!
      implicit none
!
      type(sph_mesh_data), allocatable, save :: org_sph_mesh(:)
      type(phys_data), allocatable, save ::     org_sph_phys(:)
!
      type(sph_mesh_data), allocatable, save :: new_sph_mesh(:)
      type(phys_data), allocatable, save ::     new_sph_phys(:)
!
      type(field_IO), save :: new_fst_IO
!
      type(sph_radial_itp_data), save :: r_itp
      type(rj_assemble_tbl), allocatable, save :: j_table_s(:,:)
!
      integer(kind = kint_gl), allocatable, target                      &
     &                        :: istsack_nnod_list(:)
!
      integer(kind = kint) :: nlayer_ICB_org, nlayer_CMB_org
      integer(kind = kint) :: nlayer_ICB_new, nlayer_CMB_new
!
      integer(kind = kint) :: istep
      integer(kind = kint) :: jp, ip, irank_org, irank_new
!
!
!
      call calypso_MPI_init
!
      write(*,*) 'Simulation start: PE. '
!
      call read_control_assemble_sph
      call set_control_4_newsph
!
      allocate( org_sph_mesh(np_sph_org) )
      allocate( org_sph_phys(np_sph_org) )
      allocate( new_sph_mesh(np_sph_new) )
      allocate( new_sph_phys(np_sph_new) )
!
!
!  set original spectr data
!
      iflag_sph_file_fmt = ifmt_org_sph_file
      sph_file_head = org_sph_head
      do ip = 1, np_sph_org
        irank_org = ip - 1
        call set_local_rj_mesh_4_merge(irank_org,                       &
     &      org_sph_mesh(ip)%sph_mesh, org_sph_mesh(ip)%sph_comms,      &
     &      org_sph_mesh(ip)%sph_grps)
        call set_sph_boundary_4_merge(org_sph_mesh(ip)%sph_grps,        &
     &      nlayer_ICB_org, nlayer_CMB_org)
      end do
!
!  set new spectr data
!
      iflag_sph_file_fmt = ifmt_new_sph_file
      sph_file_head = new_sph_head
      do jp = 1, np_sph_new
        irank_new = jp - 1
        call set_local_rj_mesh_4_merge(irank_new,                       &
     &      new_sph_mesh(jp)%sph_mesh, new_sph_mesh(jp)%sph_comms,      &
     &      new_sph_mesh(jp)%sph_grps)
        call set_sph_boundary_4_merge(new_sph_mesh(jp)%sph_grps,        &
     &      nlayer_ICB_new, nlayer_CMB_new)
      end do
!
!     Construct mode transfer table
!
      allocate(j_table_s(np_sph_new,np_sph_org))
      do jp = 1, np_sph_new
        do ip = 1, np_sph_org
          call alloc_each_mode_tbl_4_assemble                           &
     &      (org_sph_mesh(ip)%sph_mesh, j_table_s(jp,ip))
          call set_mode_table_4_assemble(org_sph_mesh(ip)%sph_mesh,     &
     &      new_sph_mesh(jp)%sph_mesh, j_table_s(jp,ip))
        end do
      end do
!
      allocate(istsack_nnod_list(0:np_sph_new))
      istsack_nnod_list(0) = 0
      do jp = 1, np_sph_new
        istsack_nnod_list(jp) = istsack_nnod_list(jp-1)                 &
     &                       + new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj
      end do
      new_fst_IO%istack_numnod_IO => istsack_nnod_list
!
!     Share number of nodes for new mesh
!
!     construct interpolation table
      call sph_radial_interpolation_coef                                &
     &     (org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      org_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r,             &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r, r_itp)
!
!      Construct field list from spectr file
!
      call load_field_name_assemble_sph                                 &
     &   (org_sph_fst_head, ifmt_org_sph_fst, istep_start,              &
     &    np_sph_org, new_sph_mesh(1)%sph_mesh,                         &
     &    org_sph_phys(1), new_sph_phys(1))
!
      do jp = 2, np_sph_new
        new_sph_phys(jp)%num_phys =  new_sph_phys(1)%num_phys
        new_sph_phys(jp)%ntot_phys = new_sph_phys(1)%ntot_phys
!
        call alloc_phys_name_type(new_sph_phys(jp))
        call alloc_phys_data_type                                       &
     &     (new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj, new_sph_phys(jp))
!
        new_sph_phys(jp)%num_component = new_sph_phys(1)%num_component
        new_sph_phys(jp)%istack_component                               &
     &           = new_sph_phys(1)%istack_component
        new_sph_phys(jp)%iflag_monitor = new_sph_phys(1)%iflag_monitor
        new_sph_phys(jp)%phys_name = new_sph_phys(1)%phys_name
      end do
!
!
!      do jp = 1, np_sph_new
!        do ip = 1, np_sph_org
!          do j = 1, org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(2)
!            if(j_table_s(jp,ip)%j_org_to_new(j).gt. 0)                 &
!     &        write(50+my_rank,*) ip, j,                               &
!     &        j_table_s(jp,ip)%j_org_to_new(j)
!          end do
!        end do
!      end do
!
      do istep = istep_start, istep_end, increment_step
!
!     Load original spectr data
        do ip = 1, np_sph_org
          call load_org_fld_data(org_sph_fst_head, ifmt_org_sph_fst,    &
     &        ip, istep, org_sph_mesh(ip)%sph_mesh, org_sph_phys(ip))
        end do
        time = 0.0d0
        dt =   0.0d0
        i_step_MHD = 0
!
!     Bloadcast original spectr data
        do ip = 1, np_sph_org
          do jp = 1, np_sph_new
!
!     Copy spectr data to temporal array
            if(r_itp%iflag_same_rgrid .eq. 0) then
              call r_itp_field_data_sph_assemble                        &
     &          (org_sph_mesh(ip)%sph_mesh, new_sph_mesh(jp)%sph_mesh,  &
     &           r_itp, j_table_s(jp,ip), new_sph_phys(jp)%ntot_phys,   &
     &           org_sph_phys(ip)%d_fld, new_sph_phys(jp)%d_fld)
            else
              call copy_field_data_sph_assemble&
     &          (org_sph_mesh(ip)%sph_mesh, new_sph_mesh(jp)%sph_mesh,  &
     &           j_table_s(jp,ip), new_sph_phys(jp)%ntot_phys,          &
     &           org_sph_phys(ip)%d_fld, new_sph_phys(jp)%d_fld)
            end if
!
            call copy_field_data_sph_center                             &
     &          (org_sph_mesh(ip)%sph_mesh, new_sph_mesh(jp)%sph_mesh,  &
     &           j_table_s(jp,ip), new_sph_phys(jp)%ntot_phys,          &
     &           org_sph_phys(ip)%d_fld, new_sph_phys(jp)%d_fld)
!
          end do
          call dealloc_phys_data_type(org_sph_phys(ip))
        end do
!
        call set_field_file_fmt_prefix                                  &
     &     (ifmt_new_sph_fst, new_sph_fst_head, new_fst_IO)
        do jp = 1, np_sph_new
          irank_new = jp - 1
          call const_assembled_sph_data                                 &
     &       (b_sph_ratio, new_sph_mesh(jp)%sph_mesh,                   &
     &        r_itp, new_sph_phys(jp), new_fst_IO)
!
          call sel_write_step_SPH_field_file                            &
     &       (np_sph_new, irank_new, istep, new_fst_IO)
!
          call dealloc_phys_data_IO(new_fst_IO)
          call dealloc_phys_name_IO(new_fst_IO)
        end do
!
        write(*,*) 'step', istep, 'finish '
      end do
!
      do jp = 1, np_sph_new
        do ip = 1, np_sph_org
          call dealloc_mode_table_4_assemble(j_table_s(jp,ip))
        end do
      end do
      deallocate(j_table_s)
!
      deallocate(org_sph_mesh, org_sph_phys)
      deallocate(new_sph_mesh, new_sph_phys)
!
      call calypso_MPI_finalize
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine load_org_fld_data(org_sph_fst_head, ifmt_org_sph_fst,  &
     &          ip, istep, org_sph, org_phys)
!
      use input_old_file_sel_4_zlib
      use copy_rj_phys_data_4_IO
!
      character(len=kchara), intent(in) :: org_sph_fst_head
      integer(kind=kint ), intent(in) :: ifmt_org_sph_fst
!
      integer(kind = kint), intent(in) :: ip, istep
      type(sph_grids), intent(in) :: org_sph
      type(phys_data), intent(inout) :: org_phys
!
!>      Field data IO structure for original data
      type(field_IO) :: org_fst_IO
      integer(kind = kint) :: irank_org
!
      irank_org = ip - 1
      call set_field_file_fmt_prefix                                    &
     &   (ifmt_org_sph_fst, org_sph_fst_head, org_fst_IO)
      call sel_read_alloc_field_file(irank_org, istep, org_fst_IO)
!
      call alloc_phys_data_type(org_sph%sph_rj%nnod_rj, org_phys)
      call copy_rj_phys_data_from_IO                                    &
     &       (org_sph%sph_rj%nnod_rj, org_fst_IO, org_phys)
!
      call dealloc_phys_data_IO(org_fst_IO)
      call dealloc_phys_name_IO(org_fst_IO)
!
      end subroutine load_org_fld_data
!
! ----------------------------------------------------------------------
!
      end program add_time_step_to_sph
