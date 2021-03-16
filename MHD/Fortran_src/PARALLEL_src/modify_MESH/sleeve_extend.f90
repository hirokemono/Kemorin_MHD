!> @file  sleeve_extend.f90
!!      module sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Routines to constructu elment communication table
!!
!!@verbatim
!!      subroutine extend_sleeve_loop                                   &
!!     &         (sleeve_exp_p, mesh, group, ele_comm,                  &
!!     &          newmesh, newgroup, new_ele_comm)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!        type(communication_table), intent(inout) :: ele_comm
!!      subroutine extend_mesh_sleeve                                   &
!!     &         (sleeve_exp_p, nod_comm, ele_comm, org_node, org_ele,  &
!!     &          new_nod_comm, new_node, new_ele, new_ele_comm,        &
!!     &          dist_4_comm, iflag_process_extend)
!!        type(sleeve_extension_param), intent(in) :: sleeve_exp_p
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: ele_comm
!!        type(node_data), intent(in) :: org_node
!!        type(element_data), intent(in) :: org_ele
!!        type(communication_table), intent(inout) :: new_nod_comm
!!        type(node_data), intent(inout) :: new_node
!!        type(element_data), intent(inout) :: new_ele
!!        type(communication_table), intent(inout) :: new_ele_comm
!!        type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
!!        integer(kind = kint), intent(inout) :: iflag_process_extend
!!@endverbatim
!
      module sleeve_extend
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_ctl_param_sleeve_extend
      use t_comm_table_for_each_pe
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine extend_sleeve_loop                                     &
     &         (sleeve_exp_p, mesh, group, ele_comm)
!
      use extended_groups
      use copy_mesh_structures
      use nod_and_ele_derived_info
!
      type(sleeve_extension_param), intent(inout) :: sleeve_exp_p
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(communication_table), intent(inout) :: ele_comm
!
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(communication_table), save :: new_ele_comm
!
      type(dist_from_wall_in_export) :: dist_4_comm
!
      integer(kind = kint) :: iflag_process_extend = 0
      integer(kind = kint) :: iloop
!
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
!
      dist_4_comm%ntot = mesh%nod_comm%ntot_export
      allocate(dist_4_comm%distance_in_export(dist_4_comm%ntot))
!$omp parallel workshare
      dist_4_comm%distance_in_export(1:dist_4_comm%ntot) = 0.0d0
!$omp end parallel workshare
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
      do iloop = 1, 10
        if(iflag_debug.gt.0) write(*,*) 'extend_mesh_sleeve', iloop
        call extend_mesh_sleeve                                         &
     &     (sleeve_exp_p, mesh%nod_comm, ele_comm, mesh%node, mesh%ele, &
     &      newmesh%nod_comm, newmesh%node, newmesh%ele, new_ele_comm,  &
     &      dist_4_comm, iflag_process_extend)
        call s_extended_groups                                          &
     &     (mesh, group, newmesh, new_ele_comm, newgroup)
!
        call dealloc_comm_table(ele_comm)
        call dealloc_numele_stack(mesh%ele)
        call dealloc_nod_and_ele_infos(mesh)
        call dealloc_mesh_data(mesh, group)
!
!        if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
        call alloc_sph_node_geometry(newmesh%node)
        call copy_mesh_and_group(newmesh, newgroup, mesh, group)
        call copy_comm_tbl_types(new_ele_comm, ele_comm)
        call set_nod_and_ele_infos(mesh%node, mesh%ele)
!        if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
!
        if(iflag_process_extend .eq. 0) exit
      end do
!
      end subroutine extend_sleeve_loop
!
!  ---------------------------------------------------------------------
!
      subroutine extend_mesh_sleeve                                     &
     &         (sleeve_exp_p, nod_comm, ele_comm, org_node, org_ele,    &
     &          new_nod_comm, new_node, new_ele, new_ele_comm,          &
     &          dist_4_comm, iflag_process_extend)
!
      use t_next_node_ele_4_node
      use t_para_double_numbering
      use t_repart_double_numberings
      use t_mesh_for_sleeve_extend
      use t_trim_overlapped_import
      use t_flags_each_comm_extend
      use t_mark_node_ele_to_extend
      use t_comm_table_for_each_pe
      use t_sort_data_for_sleeve_trim
!
      use set_ele_id_4_node_type
      use extend_comm_table
      use const_extended_neib_domain
      use const_nod_ele_to_extend
      use const_extend_nod_comm_table
      use const_extend_ele_comm_table
      use append_communication_table
      use append_extended_node
      use append_extended_element
      use checks_for_sleeve_extend
      use check_sleeve_extend_mesh
!
      type(sleeve_extension_param), intent(in) :: sleeve_exp_p
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: ele_comm
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
!
      type(communication_table), intent(inout) :: new_nod_comm
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
      type(communication_table), intent(inout) :: new_ele_comm
      type(dist_from_wall_in_export), intent(inout) :: dist_4_comm
      integer(kind = kint), intent(inout) :: iflag_process_extend
!
      type(node_ele_double_number), save :: inod_dbl
      type(node_ele_double_number), save :: iele_dbl
      type(node_ele_double_number), save :: dbl_id2
      type(element_around_node), save :: neib_ele
!
!>      Structure of double numbering
      type(communication_table), save :: expand_ele_comm
      type(ele_data_for_sleeve_ext), save :: exp_export_ie
      type(ele_data_for_sleeve_ext), save :: exp_import_ie
      type(ele_data_for_sleeve_ext), save :: trim_import_ie
!
      type(communication_table), save :: expand_nod_comm
      type(node_data_for_sleeve_ext), save :: exp_export_xx
      type(node_data_for_sleeve_ext), save :: exp_import_xx
      type(node_data_for_sleeve_ext), save :: trim_import_xx
!
      type(data_for_trim_import), save :: ext_nod_trim
      type(import_extend_to_trim), save :: trim_nod_to_ext
!
      type(marks_for_sleeve_extension), save :: marks_4_extend
!
      type(communication_table), save :: add_nod_comm
      type(communication_table), save :: add_ele_comm
!
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
      if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_ele_id_4_node(org_node, org_ele, neib_ele)
!
      call alloc_double_numbering(org_node%numnod, inod_dbl)
      if (iflag_debug.gt.0) write(*,*) 'set_node_double_numbering'
      call set_node_double_numbering(org_node, nod_comm, inod_dbl)
!
      call alloc_double_numbering(org_ele%numele, iele_dbl)
      call double_numbering_4_element(org_ele, ele_comm, iele_dbl)
!
      call alloc_sleeve_extension_marks(nod_comm, marks_4_extend)
      call const_sleeve_expand_list                                     &
     &   (sleeve_exp_p, nod_comm, org_node, org_ele, neib_ele,          &
     &    dist_4_comm, marks_4_extend)
      call dealloc_iele_belonged(neib_ele)
!
      call s_const_extended_neib_domain(nod_comm, inod_dbl,             &
     &    marks_4_extend%mark_nod, add_nod_comm, iflag_process_extend)
!
      call comm_extended_import_nod_ele                                 &
     &   (nod_comm, org_node, inod_dbl, org_ele, iele_dbl,              &
     &    marks_4_extend, expand_nod_comm, expand_ele_comm,             &
     &    exp_import_xx, exp_import_ie)
      call dealloc_sleeve_extension_marks(marks_4_extend)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+2)
      call const_extended_node_position                                 &
     &   (nod_comm, expand_nod_comm, exp_import_xx,                     &
     &    add_nod_comm, ext_nod_trim, trim_nod_to_ext)
!
      call const_extended_nod_comm_table                                &
     &   (org_node, expand_nod_comm, ext_nod_trim,                      &
     &    exp_import_xx, trim_import_xx, trim_nod_to_ext,               &
     &    dist_4_comm, add_nod_comm)
!
      call s_append_extended_node(org_node, inod_dbl, add_nod_comm,     &
     &    trim_import_xx, trim_nod_to_ext%import_lc_trimmed,            &
     &    new_node, dbl_id2)
      call dealloc_double_numbering(inod_dbl)
!
      call check_appended_node_data                                     &
     &   (org_node, expand_nod_comm, add_nod_comm, exp_import_xx,       &
     &    ext_nod_trim, trim_import_xx, dbl_id2,                        &
     &    trim_nod_to_ext%idx_extend_to_trim,                           &
     &    trim_nod_to_ext%import_lc_trimmed)
      deallocate(trim_nod_to_ext%import_lc_trimmed)
!
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'start new_nod_comm'
      call s_append_communication_table                                 &
     &   (nod_comm, add_nod_comm, new_nod_comm)
      call check_new_node_and_comm(new_nod_comm, new_node, dbl_id2)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+2)
!
!
!
!      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+3)
      call const_extended_element_connect                               &
     &   (nod_comm, org_node, org_ele, dbl_id2,                         &
     &    expand_nod_comm, add_nod_comm, exp_import_xx, ext_nod_trim,   &
     &    trim_nod_to_ext%idx_extend_to_trim,                           &
     &    expand_ele_comm, exp_import_ie)
      deallocate(trim_nod_to_ext%idx_extend_to_trim)
      call dealloc_node_data_sleeve_ext(exp_import_xx)
      call dealloc_stack_to_trim_extend(ext_nod_trim)
      call dealloc_idx_trimed_to_sorted(ext_nod_trim)
!
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'const_extended_ele_comm_table'
      call const_extended_ele_comm_table                                &
     &   (nod_comm, org_ele, add_nod_comm, expand_ele_comm,             &
     &    exp_import_ie, trim_import_ie, add_ele_comm)
      call dealloc_comm_table(expand_ele_comm)
!
      call s_append_communication_table                                 &
     &   (ele_comm, add_ele_comm, new_ele_comm)
      call s_append_extended_element(org_ele, add_ele_comm,             &
     &    trim_import_ie, new_ele)
!
      call check_returned_extend_element                                &
     &   (iele_dbl, add_ele_comm, trim_import_ie)
      call dealloc_double_numbering(iele_dbl)
!
      call check_extended_element                                       &
     &   (new_nod_comm, new_node, new_ele, new_ele_comm)
!      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+3)
!
      end subroutine extend_mesh_sleeve
!
!  ---------------------------------------------------------------------
!
      end module sleeve_extend
