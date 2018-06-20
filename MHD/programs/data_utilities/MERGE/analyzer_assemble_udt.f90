!>@file   analyzer_assemble_udt.f90
!!@brief  module analyzer_assemble_udt
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_assemble_udt
!!      subroutine analyze_assemble_udt
!!@endverbatim
!
      module analyzer_assemble_udt
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_geometry_data_4_merge
      use m_machine_parameter
      use m_control_param_newsph
      use r_interpolate_marged_sph
      use t_SPH_mesh_field_data
      use t_field_data_IO
      use t_assembled_field_IO
!
      use new_SPH_restart
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
!
      use t_mesh_data
      use t_time_data
      use t_ucd_data
!
      implicit none
!
      type(time_data), save :: init_t
!
      type(mesh_geometry), allocatable, save :: org_mesh(:)
      type(mesh_geometry), save :: new_mesh
      type(ucd_data), save :: second_ucd
!
      integer(kind = kint), allocatable :: istack_recv(:)
      integer(kind = kint), allocatable :: item_send(:)
      integer(kind = kint), allocatable :: item_recv(:)
!>        Instance for FEM field data IO
      type(time_data), save :: fem_time_IO
!
!
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
      integer(kind = kint), allocatable :: nnod_list_lc(:)
      integer(kind = kint), allocatable :: nnod_list(:)
      integer(kind = kint_gl), allocatable :: istack_nnod_list(:)
!
!
      type(sph_radial_itp_data), save :: r_itp
      type(rj_assemble_tbl), allocatable, save :: j_table(:,:)
      integer(kind = kint) :: nlayer_ICB_org, nlayer_CMB_org
      integer(kind = kint) :: nlayer_ICB_new, nlayer_CMB_new
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_assemble_udt
!
      use m_error_IDs
      use m_control_param_merge
      use m_control_data_4_merge
      use m_array_for_send_recv
!
      use m_original_ucd_4_merge
      use mpi_load_mesh_data
      use load_mesh_data_4_merge
      use search_original_domain_node
      use output_newdomain_ucd
      use nod_phys_send_recv
!
      integer(kind = kint) :: ip, jp, irank_new, jloop, inod
      integer(kind = kint_gl) :: min_inod_gl, max_inod_gl
      integer(kind = kint) :: nnod_4_surf, nnod_4_edge
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      if(my_rank .eq. 0) then
        write(*,*) ' Do you prepare folloing data???'
        write(*,*) ' original mesh data:  mesh/in.PE#'
        write(*,*) ' transfered mesh data:  mesh_target/in.PE#'
        write(*,*) ' control data for this routine:  control_merge'
        write(*,*) ' simulation results: field/out.step#.PE#.udt'
        write(*,*) ' transfered results: field_new/out.step#.PE#.udt'
      end if
!
!   read control data
!
      call read_control_4_merge
!
      call set_control_4_merge(mgd_mesh1%num_pe)
      call set_control_4_newudt(sec_mesh1%num_pe2)
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           istep_start, istep_end, increment_step
!
      allocate( org_sph_phys(np_sph_org) )
      allocate( new_sph_mesh(np_sph_new) )
      allocate( new_sph_phys(np_sph_new) )
      allocate(j_table(np_sph_org,np_sph_new))
!
!  set new mesh data
!
      call mpi_input_mesh_geometry                                      &
     &   (nprocs, merged_mesh_file, new_mesh, nnod_4_surf, nnod_4_edge)
!
!  set original mesh data
!
      allocate( org_mesh(mgd_mesh1%num_pe) )
      call load_local_node_4_merge                                      &
     &   (merge_org_mesh_file, mgd_mesh1%num_pe, org_mesh)
!
      allocate(istack_recv(0:mgd_mesh1%num_pe))
      allocate(item_send(new_mesh%node%internal_node))
      allocate(item_recv(new_mesh%node%internal_node))
!
      istack_recv = 0
!$omp parallel workshare
      item_send = 0
      item_recv = 0
!$omp end parallel workshare
!
      call s_search_original_domain_node(mgd_mesh1%num_pe, org_mesh,    &
     &    new_mesh%node, istack_recv, item_send, item_recv)
!
!   read field name and number of components
!
      write(*,*) 'init_ucd_data_4_merge'
      call init_ucd_data_4_merge                                        &
     &   (istep_start, original_ucd_param, fem_time_IO)
!
!    set list array for merged field
!
      call set_field_list_4_merge(mgd_mesh1%merged_fld)
      write(*,*) 'set_field_list_4_merge'
      call alloc_phys_data_type                                         &
     &   (new_mesh%node%numnod, mgd_mesh1%merged_fld)
!
      write(*,*) 'assemble_2nd_udt_mesh'
      call assemble_2nd_udt_mesh                                        &
     &   (assemble_ucd_param, mgd_mesh1%merged, my_rank,                &
     &    new_mesh, second_ucd)
!
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, new_mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(new_mesh)
!
      end subroutine init_assemble_udt
!
! ----------------------------------------------------------------------
!
      subroutine analyze_assemble_udt
!
      use m_phys_labels
      use m_control_param_merge
      use search_original_domain_node
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: ip, jp, irank_new
      integer(kind = kint) :: iloop, jloop
      integer(kind = kint) :: istep_out
!
      type(phys_data) :: new_fld
      type(field_IO) :: new_fIO
!
      call alloc_phys_name_type(new_fld)
      call alloc_phys_data_type(new_mesh%node%numnod, new_fld)
!
      call alloc_phys_data_IO(new_fIO)
      call alloc_merged_field_stack(nprocs, new_fIO)
!      call count_number_of_node_stack                                   &
!     &   (new_fIO%nnod_IO, new_fIO%istack_numnod_IO)
!
!
!      do istep = istep_start, istep_end, increment_step
!        call assemble_field_data                                        &
!     &     (istep, mgd_mesh1%num_pe, new_mesh, ifield_2_copy,           &
!     &      istack_recv, item_send, item_recv,                          &
!     &      original_ucd_param, assemble_ucd_param, new_fld, new_fIO)
!      end do
!      deallocate(j_table)
!
      deallocate(istack_recv, item_send, item_recv)
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_assemble_udt
!
! ----------------------------------------------------------------------
!
      end module analyzer_assemble_udt
