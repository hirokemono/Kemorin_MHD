!> @file  parallel_sleeve_extension.f90
!!      module parallel_sleeve_extension
!!
!! @author  H. Matsui
!! @date Programmed in June, 2018
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine para_sleeve_extension(mesh, group)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!      subroutine elpsed_label_4_sleeve_ext
!!@endverbatim
!
      module parallel_sleeve_extension
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_group_data
      use t_comm_table
      use m_work_time
!
      implicit none
!
      logical, save :: iflag_SLEX_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_SLEX =   0
      integer(kind = kint), save :: ied_elapsed_SLEX =   0
!
      private :: iflag_SLEX_time, ist_elapsed_SLEX, ied_elapsed_SLEX
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine para_sleeve_extension(mesh, group)
!
      use t_para_double_numbering
      use t_next_node_ele_4_node
!
      use const_mesh_information
      use const_element_comm_tables
      use set_table_4_RHS_assemble
      use extend_comm_table
      use extend_element_connect
      use extend_group_table
      use copy_mesh_structures
      use set_nnod_4_ele_by_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(communication_table), save :: ele_comm
      type(communication_table), save :: new_ele_comm
      type(mesh_geometry), save :: newmesh
      type(mesh_groups), save :: newgroup
      type(next_nod_ele_table), save :: next_tbl
      type(parallel_double_numbering), save :: dbl_id1
!
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+1)
      call const_mesh_infos(my_rank, mesh, group)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbl_only'
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
      call const_element_comm_tbl_only(mesh, ele_comm)
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
!
      if (iflag_debug.gt.0) write(*,*) 'set_belonged_ele_and_next_nod'
      call set_belonged_ele_and_next_nod                                &
     &   (mesh, next_tbl%neib_ele, next_tbl%neib_nod)
!
      call alloc_double_numbering(mesh%node%numnod, dbl_id1)
      if (iflag_debug.gt.0) write(*,*) 'set_para_double_numbering'
      call set_para_double_numbering                                    &
     &   (mesh%node%internal_node, mesh%nod_comm, dbl_id1)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+1)
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+2)
      if (iflag_debug.gt.0) write(*,*) 'extend_node_comm_table'
      call extend_node_comm_table                                       &
     &   (mesh%nod_comm, mesh%node, dbl_id1, next_tbl%neib_nod,         &
     &    newmesh%nod_comm, newmesh%node)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+2)
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+3)
      if (iflag_debug.gt.0) write(*,*) 'extend_ele_connectivity'
      call extend_ele_connectivity                                      &
     &   (mesh%nod_comm, ele_comm, mesh%node, mesh%ele,                 &
     &    dbl_id1, next_tbl%neib_ele, newmesh%nod_comm, newmesh%node,   &
     &    newmesh%ele)
      newmesh%ele%first_ele_type                                        &
     &   = set_cube_eletype_from_num(newmesh%ele%nnod_4_ele)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+3)
!
      call dealloc_double_numbering(dbl_id1)
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+4)
      call alloc_sph_node_geometry(newmesh%node)
      call set_nod_and_ele_infos(newmesh%node, newmesh%ele)
!
      if(iflag_SLEX_time) call start_elapsed_time(ist_elapsed_SLEX+5)
      call const_element_comm_tbl_only(newmesh, new_ele_comm)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+5)
!
      if (iflag_debug.gt.0) write(*,*) 's_extend_group_table'
      call s_extend_group_table                                         &
     &   (nprocs, newmesh%nod_comm, new_ele_comm,                       &
     &    newmesh%node, newmesh%ele, group, newgroup)
      if(iflag_SLEX_time) call end_elapsed_time(ist_elapsed_SLEX+4)
!
      call dealloc_next_nod_ele_table(next_tbl)
      call dealloc_comm_table(ele_comm)
      call dealloc_numele_stack(mesh%ele)
      call dealloc_mesh_infomations(mesh, group)
      call dealloc_mesh_smp_stack(mesh, group)
      call dealloc_mesh_data(mesh, group)
!
      if (iflag_debug.gt.0) write(*,*) 'copy_mesh_and_group'
      call copy_mesh_and_group(newmesh, newgroup, mesh, group)
!
      call dealloc_comm_table(new_ele_comm)
      call dealloc_numele_stack(newmesh%ele)
      call dealloc_overlapped_ele(newmesh%ele)
      call dealloc_ele_geometry(newmesh%ele)
      call dealloc_ele_param_smp(newmesh%ele)
      call dealloc_node_param_smp(newmesh%node)
      call dealloc_mesh_data(newmesh, newgroup)
!
      end subroutine para_sleeve_extension
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_sleeve_ext
!
      integer(kind = kint), parameter :: num_append = 5
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SLEX, ied_elapsed_SLEX)
!
      elps1%labels(ist_elapsed_SLEX+1)                                  &
     &                    = 'Sleeve extension preparation'
      elps1%labels(ist_elapsed_SLEX+2)                                  &
     &                    = 'extend_node_comm_table'
      elps1%labels(ist_elapsed_SLEX+3)                                  &
     &                    = 'extend_ele_connectivity'
      elps1%labels(ist_elapsed_SLEX+4)                                  &
     &                    = 'Construct groups for extended  '
      elps1%labels(ist_elapsed_SLEX+5)                                  &
     &                    = 'element comm. table in sleeve extension  '
!
      iflag_SLEX_time = .TRUE.
!
      end subroutine elpsed_label_4_sleeve_ext
!
!-----------------------------------------------------------------------
!
      end module parallel_sleeve_extension
