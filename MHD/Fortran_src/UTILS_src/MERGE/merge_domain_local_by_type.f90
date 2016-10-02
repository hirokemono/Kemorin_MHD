!merge_domain_local_by_type.f90
!      module merge_domain_local_by_type
!
!      Written by Kemorin on May, 2010
!
!      subroutine set_domain_local_id_by_type1(nprocs, mesh_info)
!      subroutine set_domain_local_id_by_type2(nprocs, mesh_info)
!
      module merge_domain_local_by_type
!
      use m_precision
!
      use t_mesh_data
!
      implicit none
!
      private :: set_domain_local_id_by_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_domain_local_id_by_type1(nprocs, mesh_info)
!
      use m_geometry_data_4_merge
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data_p), intent(in) :: mesh_info(nprocs)
!
!
      call set_domain_local_id_by_type(nprocs, mesh_info,               &
     &    merge_tbl%nnod_overlap, merge_tbl%nele_overlap,               &
     &    merge_tbl%inod_local, merge_tbl%idomain_nod,                  &
     &    merge_tbl%iele_local, merge_tbl%idomain_ele)
!
      end subroutine set_domain_local_id_by_type1
!
!  ---------------------------------------------------------------------
!
      subroutine set_domain_local_id_by_type2(nprocs, mesh_info)
!
      use m_2nd_geometry_4_merge
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data_p), intent(in) :: mesh_info(nprocs)
!
!
      call set_domain_local_id_by_type(nprocs, mesh_info,               &
     &    merge_tbl_2%nnod_overlap, merge_tbl_2%nele_overlap,           &
     &    merge_tbl_2%inod_local, merge_tbl_2%idomain_nod,              &
     &    merge_tbl_2%iele_local, merge_tbl_2%idomain_ele)
!
      end subroutine set_domain_local_id_by_type2
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_domain_local_id_by_type(nprocs, mesh_info,         &
     &          nodpetot, elmpetot, inod_local, idomain_nod,            &
     &          iele_local, idomain_ele)
!
      integer(kind = kint), intent(in) :: nprocs
      type(mesh_data_p), intent(in) :: mesh_info(nprocs)
!
      integer(kind = kint), intent(in) :: nodpetot, elmpetot
      integer(kind = kint), intent(inout) :: inod_local(nodpetot)
      integer(kind = kint), intent(inout) :: idomain_nod(nodpetot)
      integer(kind = kint), intent(inout) :: iele_local(elmpetot)
      integer(kind = kint), intent(inout) :: idomain_ele(elmpetot)
!
      integer(kind = kint) :: ip, inod, iele
      integer(kind = kint_gl) :: inod_gl, iele_gl
!
!
      do ip = 1, nprocs
        do inod = 1, mesh_info(ip)%mesh%node%internal_node
          inod_gl = mesh_info(ip)%mesh%node%inod_global(inod)
          inod_local(inod_gl) = inod
          idomain_nod(inod_gl) = ip
        end do
        do iele = 1, mesh_info(ip)%mesh%ele%numele
          if( mesh_info(ip)%mesh%ele%interior_ele(iele) .gt. 0) then
            iele_gl = mesh_info(ip)%mesh%ele%iele_global(iele)
            iele_local(iele_gl) = iele
            idomain_ele(iele_gl) = ip
          end if
        end do
      end do
!
      end subroutine set_domain_local_id_by_type
!
!   --------------------------------------------------------------------
!
      end module merge_domain_local_by_type
