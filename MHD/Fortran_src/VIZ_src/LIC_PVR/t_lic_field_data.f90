!>@file  t_lic_field_data.f90
!!       module t_lic_field_data
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine alloc_nod_vector_4_lic                               &
!!     &         (numnod, num_masking, field_lic)
!!      subroutine dealloc_nod_data_4_lic(field_lic)
!!      subroutine cal_field_4_each_lic                                 &
!!     &         (node, nod_fld, lic_p, field_lic)
!!        type(node_data), intent(in) :: node
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(phys_data), intent(in) :: nod_fld
!!        type(lic_field_data), intent(inout) :: field_lic
!!      subroutine repartition_lic_field                                &
!!     &         (node, repart_p, viz_mesh, mesh_to_viz_tbl,            &
!!     &          nod_fld_lic, field_lic, v_sol)
!!        type(node_data), intent(in) :: node
!!        type(volume_partioning_param), intent(in) :: repart_p
!!        type(mesh_geometry), intent(in) :: viz_mesh
!!        type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
!!        type(lic_field_data), intent(inout) :: nod_fld_lic
!!        type(lic_field_data), intent(inout) :: field_lic
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
!
      module t_lic_field_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>  Structure for field data for LIC
      type lic_field_data
!>    Data for rendering
        real(kind = kreal), allocatable :: d_lic(:)
!
!>    Vector Data for LIC
        real(kind = kreal), allocatable :: v_lic(:,:)
!>    Vector Data for LIC opacity
!        real(kind = kreal), allocatable :: o_lic(:)
!>    Number of LIC masking data fiels
        integer(kind = kint) :: num_mask = 0
!>    Vector Data for LIC masking data
        real(kind = kreal), allocatable :: s_lic(:,:)
      end type lic_field_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_vector_4_lic                                 &
     &         (numnod, num_masking, field_lic)
!
      integer(kind = kint), intent(in) :: numnod, num_masking
      type(lic_field_data), intent(inout) :: field_lic
!
!
      allocate(field_lic%d_lic(numnod))
      allocate(field_lic%v_lic(numnod,3))
!      allocate(field_lic%o_lic(numnod))
      if(numnod .gt. 0) field_lic%d_lic =    0.0d0
      if(numnod .gt. 0) field_lic%v_lic =    0.0d0
!      if(numnod .gt. 0) field_lic%o_lic =    0.0d0
!
      field_lic%num_mask = num_masking
      allocate(field_lic%s_lic(numnod,field_lic%num_mask))
      if(numnod*num_masking .gt. 0) field_lic%s_lic = 0.0d0
!
      end subroutine alloc_nod_vector_4_lic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_data_4_lic(field_lic)
!
      type(lic_field_data), intent(inout) :: field_lic
!
!
      deallocate(field_lic%v_lic)
!      deallocate(field_lic%o_lic)
!
      deallocate(field_lic%s_lic)
      deallocate(field_lic%d_lic)
!
      end subroutine dealloc_nod_data_4_lic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_field_4_each_lic                                   &
     &         (node, nod_fld, lic_p, field_lic)
!
      use m_error_IDs
      use t_control_param_LIC
      use t_geometry_data
      use t_phys_data
      use convert_components_4_viz
      use set_components_flags
!
      type(node_data), intent(in) :: node
      type(lic_parameters), intent(in) :: lic_p
      type(phys_data), intent(in) :: nod_fld
!
      type(lic_field_data), intent(inout) :: field_lic
!
      integer(kind = kint) :: i_field, ist_fld, num_comp, i
!
!
      i_field =  lic_p%lic_field%id_field
      ist_fld =  nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    icomp_VECTOR, nod_fld%d_fld(1,ist_fld+1),                     &
     &    field_lic%v_lic)
!
      i_field =  lic_p%lic_field%id_field
      ist_fld =  nod_fld%istack_component(i_field-1)
      num_comp = nod_fld%istack_component(i_field) - ist_fld
      call convert_comps_4_viz                                          &
     &   (node%numnod, node%istack_nod_smp, node%xx, node%rr,           &
     &    node%a_r, node%ss, node%a_s, ione, num_comp,                  &
     &    icomp_NORM, nod_fld%d_fld(1,ist_fld+1), field_lic%d_lic)
!
      if(lic_p%iflag_color_mode .eq. iflag_from_control) then
        i_field =  lic_p%color_field%id_field
        ist_fld =  nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
        call convert_comps_4_viz                                        &
     &     (node%numnod, node%istack_nod_smp, node%xx, node%rr,         &
     &      node%a_r, node%ss, node%a_s, ione, num_comp,                &
     &      lic_p%color_field%id_component, nod_fld%d_fld(1,ist_fld+1), &
     &      field_lic%d_lic)
      end if
!
      do i = 1, lic_p%num_masking
        if(lic_p%masking(i)%mask_type .eq. iflag_fieldmask) then
          i_field =  lic_p%masking(i)%field_info%id_field
          ist_fld =  nod_fld%istack_component(i_field-1)
          num_comp = nod_fld%istack_component(i_field) - ist_fld
          call convert_comps_4_viz                                      &
       &     (node%numnod, node%istack_nod_smp, node%xx, node%rr,       &
       &      node%a_r, node%ss, node%a_s, ione, num_comp,              &
       &      lic_p%masking(i)%field_info%id_component,                 &
       &      nod_fld%d_fld(1,ist_fld+1), field_lic%s_lic(1,i))
        end if
      end do
!
      end subroutine cal_field_4_each_lic
!
!  ---------------------------------------------------------------------
!
      subroutine repartition_lic_field                                  &
     &         (node, repart_p, viz_mesh, mesh_to_viz_tbl,              &
     &          nod_fld_lic, field_lic, v_sol)
!
      use m_error_IDs
      use t_geometry_data
      use t_phys_data
      use t_vector_for_solver
      use select_copy_from_recv
      use field_to_new_partition
      use transfer_to_new_partition
!
      type(node_data), intent(in) :: node
!
      type(volume_partioning_param), intent(in) :: repart_p
      type(mesh_geometry), intent(in) :: viz_mesh
      type(calypso_comm_table), intent(in) :: mesh_to_viz_tbl
!
      type(lic_field_data), intent(inout) :: nod_fld_lic
      type(lic_field_data), intent(inout) :: field_lic
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: i
!
!
      if(repart_p%flag_repartition) then
        call vector_to_new_partition                                    &
     &     (iflag_import_item, mesh_to_viz_tbl, viz_mesh%nod_comm,      &
     &      node%numnod, viz_mesh%node%numnod,                          &
     &      nod_fld_lic%v_lic, field_lic%v_lic, v_sol)
        call scalar_to_new_partition                                    &
     &     (iflag_import_item, mesh_to_viz_tbl, viz_mesh%nod_comm,      &
     &      node%numnod, viz_mesh%node%numnod,                          &
     &      nod_fld_lic%d_lic, field_lic%d_lic, v_sol)
!        call scalar_to_new_partition                                   &
!     &     (iflag_import_item, mesh_to_viz_tbl, viz_mesh%nod_comm,     &
!     &      node%numnod, viz_mesh%node%numnod,                         &
!     &      nod_fld_lic%o_lic, field_lic%o_lic, v_sol)
!
        do i = 1, field_lic%num_mask
          call scalar_to_new_partition                                  &
     &       (iflag_import_item, mesh_to_viz_tbl, viz_mesh%nod_comm,    &
     &        node%numnod, viz_mesh%node%numnod,                        &
     &        nod_fld_lic%s_lic(1,i), field_lic%s_lic(1,i), v_sol)
        end do
      end if
!
      end subroutine repartition_lic_field
!
!  ---------------------------------------------------------------------
!
      end module t_lic_field_data
