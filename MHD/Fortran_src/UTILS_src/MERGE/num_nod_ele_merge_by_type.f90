!num_nod_ele_merge_by_type.f90
!      module num_nod_ele_merge_by_type
!
!      Written by H. Matsui on Feb., 2007
!
!!      subroutine set_num_nod_ele_merge_type_wop1                      &
!!     &         (nprocs, mesh_info, mgd_mesh)
!!      subroutine set_num_nod_ele_merge_type_wop2(nprocs, mesh_info)
!!
!!      subroutine set_num_nod_ele_merge_type1                          &
!!     &         (nprocs, mesh_info, mgd_mesh)
!!        type(mesh_data), intent(in) :: mesh_info(nprocs)
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!      subroutine set_num_nod_ele_merge_type2(nprocs, mesh_info)
!
      module num_nod_ele_merge_by_type
!
      use m_precision
!
      use m_constants
      use t_mesh_data
      use t_mesh_data_with_pointer
!
      implicit none
!
      private :: set_num_nod_ele_merge_type
      private :: set_num_nod_ele_merge_type_wop
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_nod_ele_merge_type_wop1                        &
     &         (nprocs, mesh_info, mgd_mesh)
!
      use t_mesh_data_4_merge
      use count_number_with_overlap
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(in) :: mesh_info(nprocs)
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      mgd_mesh%num_pe = nprocs
      call alloc_number_of_mesh(mgd_mesh)
!
      call set_num_nod_ele_merge_type_wop                               &
     &   (mgd_mesh%num_pe, mesh_info, mgd_mesh%subdomain)
      call count_num_overlap_geom_type                                  &
     &   (mgd_mesh%num_pe, mgd_mesh%subdomain, mgd_mesh%merge_tbl)
      call count_num_geometry_w_overlap                                 &
     &   (mgd_mesh%num_pe, mgd_mesh%subdomain, mgd_mesh%merge_tbl,      &
     &    mgd_mesh%merged)
!
      call alloc_geometry_data_4_merge(mgd_mesh)
!
      end subroutine set_num_nod_ele_merge_type_wop1
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_nod_ele_merge_type_wop2(nprocs, mesh_info)
!
      use m_2nd_geometry_4_merge
      use set_2nd_geometry_4_serial
      use count_number_with_overlap
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(in) :: mesh_info(nprocs)
!
!
      sec_mesh1%num_pe2 = nprocs
      call allocate_number_of_2nd_mesh
!
      call set_num_nod_ele_merge_type_wop(nprocs, mesh_info,            &
     &    sec_mesh1%subdomains_2)
      call count_num_overlap_geom_type(sec_mesh1%num_pe2,               &
     &    sec_mesh1%subdomains_2, merge_tbl_2)
!
      call allocate_2nd_merged_geometry
      call allocate_2nd_merge_table
!
      end subroutine set_num_nod_ele_merge_type_wop2
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_nod_ele_merge_type1                            &
     &         (nprocs, mesh_info, mgd_mesh)
!
      use t_mesh_data_4_merge
      use count_number_with_overlap
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(in) :: mesh_info(nprocs)
!
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      mgd_mesh%num_pe = nprocs
      call alloc_number_of_mesh(mgd_mesh)
!
      call set_num_nod_ele_merge_type                                   &
     &   (mgd_mesh%num_pe, mesh_info, mgd_mesh%subdomain)
      call count_num_overlap_geom_type                                  &
     &   (mgd_mesh%num_pe, mgd_mesh%subdomain, mgd_mesh%merge_tbl)
      call count_num_geometry_w_overlap                                 &
     &   (mgd_mesh%num_pe, mgd_mesh%subdomain, mgd_mesh%merge_tbl,      &
     &    mgd_mesh%merged)
!
      call alloc_geometry_data_4_merge(mgd_mesh)
!
      end subroutine set_num_nod_ele_merge_type1
!
!  ---------------------------------------------------------------------
!
      subroutine set_num_nod_ele_merge_type2(nprocs, mesh_info)
!
      use m_2nd_geometry_4_merge
      use set_2nd_geometry_4_serial
      use count_number_with_overlap
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(in) :: mesh_info(nprocs)
!
!
      sec_mesh1%num_pe2 = nprocs
      call allocate_number_of_2nd_mesh
!
      call set_num_nod_ele_merge_type                                   &
     &   (nprocs, mesh_info, sec_mesh1%subdomains_2)
      call count_num_overlap_geom_type(sec_mesh1%num_pe2,               &
     &    sec_mesh1%subdomains_2, merge_tbl_2)
!
      call allocate_2nd_merged_geometry
      call allocate_2nd_merge_table
!
      end subroutine set_num_nod_ele_merge_type2
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_num_nod_ele_merge_type_wop(nprocs, mesh_info,      &
     &          subdomain)
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(in) :: mesh_info(nprocs)
!
      type(mesh_geometry), intent(inout) :: subdomain(nprocs)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs
        subdomain(ip)%node%internal_node                                &
     &       = mesh_info(ip)%mesh%node%internal_node
        subdomain(ip)%ele%numele = mesh_info(ip)%mesh%ele%numele
        subdomain(ip)%node%numnod = mesh_info(ip)%mesh%node%numnod
      end do
!
      end subroutine set_num_nod_ele_merge_type_wop
!
!------------------------------------------------------------------
!
      subroutine set_num_nod_ele_merge_type(nprocs, mesh_info,          &
     &          subdomain)
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data), intent(in) :: mesh_info(nprocs)
!
      type(mesh_geometry), intent(inout) :: subdomain(nprocs)
!
      integer(kind = kint) :: ip, iflag, iele
!
!
      do ip = 1, nprocs
        subdomain(ip)%node%internal_node                                &
     &       = mesh_info(ip)%mesh%node%internal_node
        subdomain(ip)%node%numnod = mesh_info(ip)%mesh%node%numnod
      end do
!
      do ip = 1, nprocs
        subdomain(ip)%ele%numele = 0
        do iele = 1, mesh_info(ip)%mesh%ele%numele
          iflag =  mesh_info(ip)%mesh%ele%interior_ele(iele)
          subdomain(ip)%ele%numele = subdomain(ip)%ele%numele + iflag
        end do
      end do
!
      end subroutine set_num_nod_ele_merge_type
!
!------------------------------------------------------------------
!
      end module num_nod_ele_merge_by_type
