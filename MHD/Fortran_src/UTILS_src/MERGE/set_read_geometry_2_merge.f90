!
!      module set_read_geometry_2_merge
!
!      Written by H. Matsui on Dec., 2006
!
!!      subroutine allocate_ioverlap_ele(merged)
!!      subroutine allocate_ioverlap_nod(merged)
!!      subroutine deallocate_ioverlap_ele
!!
!!      subroutine copy_read_nodal_data_2_merge                         &
!!     &         (ip, subdomain, merge_tbl, merged)
!!      subroutine copy_read_ele_data_2_merge                           &
!!     &         (ip, subdomain, merge_tbl, merged)
!!        type(mesh_geometry), intent(in) :: subdomain
!!        type(merged_stacks), intent(inout) :: merge_tbl
!!        type(mesh_geometry), intent(inout) :: merged
!!
!!      subroutine copy_udt_field_data_merge(ip, ifield_2_copy,         &
!!     &          org_fld, ucd, subdomain, merge_tbl, merged_fld)
!!        type(phys_data), intent(in) :: org_fld
!!        type(ucd_data), intent(in) :: ucd
!!        type(mesh_geometry), intent(in) :: subdomain
!!        type(phys_data), intent(inout) :: merged_fld
!
      module set_read_geometry_2_merge
!
      use m_precision
!
      use t_mesh_data
      use t_merged_geometry_data
!
      implicit none
!
      integer(kind=kint ), allocatable ::  ioverlap_n(:)
      integer(kind=kint ), allocatable ::  ioverlap_e(:)
      private :: ioverlap_n
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ioverlap_ele(merged)
!
      type(mesh_geometry), intent(in) :: merged
!
      allocate (ioverlap_e(merged%ele%numele) )
      ioverlap_e  = 0 
!
      end subroutine allocate_ioverlap_ele
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ioverlap_nod(merged)
!
      type(mesh_geometry), intent(in) :: merged
!
      allocate (ioverlap_n(merged%node%numnod) )
      ioverlap_n  = 0 
!
      end subroutine allocate_ioverlap_nod
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ioverlap_ele
!
      deallocate(ioverlap_e)
!
      end subroutine deallocate_ioverlap_ele
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ioverlap_nod
!
      deallocate(ioverlap_n)
!
      end subroutine deallocate_ioverlap_nod
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_read_nodal_data_2_merge                           &
     &         (ip, subdomain, merge_tbl, merged)
!
      integer(kind = kint), intent(in) :: ip
      type(mesh_geometry), intent(in) :: subdomain
!
      type(merged_stacks), intent(inout) :: merge_tbl
      type(mesh_geometry), intent(inout) :: merged
!
      integer(kind = kint)  :: inod, inum, j, jp, ist, ied
      integer(kind = kint_gl) :: inod_gl
!
!
      do inod = 1, subdomain%node%numnod
        inod_gl = subdomain%node%inod_global(inod)
        if (ioverlap_n(inod_gl) .eq. 0) then
          merge_tbl%nnod_merged  =  merge_tbl%nnod_merged  + 1
          merged%node%xx(inod_gl,1:3) = subdomain%node%xx(inod,1:3)
        end if
        ioverlap_n(inod_gl) = ioverlap_n(inod_gl) + 1
      end do
!
      do inod = 1, subdomain%node%internal_node
        inod_gl = subdomain%node%inod_global(inod)
        merge_tbl%inod_local(inod_gl) = inod
        merge_tbl%idomain_nod(inod_gl) = ip
      end do
!
      do j = 1, subdomain%nod_comm%num_neib
        jp = subdomain%nod_comm%id_neib(j)
        ist = subdomain%nod_comm%istack_import(j-1) + 1
        ied = subdomain%nod_comm%istack_import(j  )
        do inum = ist, ied
          inod = subdomain%nod_comm%item_import(inum)
          inod_gl = subdomain%node%inod_global(inod)
          if (merge_tbl%inod_local(inod_gl) .eq. 0) then
            merge_tbl%inod_local(inod_gl) = inod
            merge_tbl%idomain_nod(inod_gl) = jp
          end if
        end do
      end do
!
      end subroutine copy_read_nodal_data_2_merge
!
!  ---------------------------------------------------------------------
!
      subroutine copy_read_ele_data_2_merge                             &
     &         (ip, subdomain, merge_tbl, merged)
!
      integer(kind = kint), intent(in) :: ip
      type(mesh_geometry), intent(in) :: subdomain
!
      type(merged_stacks), intent(inout) :: merge_tbl
      type(mesh_geometry), intent(inout) :: merged
!
      integer(kind = kint) :: inod, iele, k
      integer(kind = kint_gl) :: iele_gl
!
!
      do iele = 1, subdomain%ele%numele
        iele_gl = subdomain%ele%iele_global(iele)
!
        if(ioverlap_e(iele_gl) .eq. 0 ) then
          merge_tbl%nele_merged  =  merge_tbl%nele_merged  + 1
          merged%ele%elmtyp(iele_gl) = subdomain%ele%elmtyp(iele)
          do k = 1, merged%ele%nnod_4_ele
            inod = subdomain%ele%ie(iele,k)
            merged%ele%ie(iele_gl,k)                                    &
     &           = int(subdomain%node%inod_global(inod))
          end do
          merge_tbl%iele_local(iele_gl) = iele
          merge_tbl%idomain_ele(iele_gl) = ip
        end if
!
        ioverlap_e(iele_gl) = ioverlap_e(iele_gl) + 1
      end do
!
      do iele =1, subdomain%ele%numele
        iele_gl = subdomain%ele%iele_global(iele)
        inod =   merged%ele%ie(iele_gl,1)
        if( merge_tbl%idomain_nod(inod) .eq. ip) then
            merge_tbl%iele_local( iele_gl ) =  iele
            merge_tbl%idomain_ele( iele_gl ) = ip
        else if( merge_tbl%idomain_nod(inod) .gt. 0                     &
     &     .and. merge_tbl%iele_local(iele_gl) .eq. 0) then
            merge_tbl%iele_local( iele_gl ) = iele
            merge_tbl%idomain_ele( iele_gl )                            &
     &         =  merge_tbl%idomain_nod(inod)
        end if
      end do
!
      end subroutine copy_read_ele_data_2_merge
!
!  ---------------------------------------------------------------------
!
      subroutine copy_udt_field_data_merge(ip, ifield_2_copy,           &
     &          org_fld, ucd, subdomain, merge_tbl, merged_fld)
!
      use t_phys_data
      use t_ucd_data
!
      type(phys_data), intent(in) :: org_fld
      type(ucd_data), intent(in) :: ucd
      type(mesh_geometry), intent(in) :: subdomain
      type(merged_stacks), intent(in) :: merge_tbl
      integer(kind = kint), intent(in) :: ip
      integer(kind=kint), intent(in) :: ifield_2_copy(org_fld%num_phys)
!
      type(phys_data), intent(inout) :: merged_fld
!
      integer(kind = kint) :: i, ic0, ic, j, nd
      integer(kind = kint) :: inod
      integer(kind = kint_gl) :: inod_gl
!
!
      do inod = 1, subdomain%node%numnod
        inod_gl = subdomain%node%inod_global(inod)
!
        if    (ioverlap_n(inod_gl) .ge. 1 ) then
          if(merge_tbl%idomain_nod(inod_gl) .eq. ip) then
            do  j = 1, org_fld%num_phys
              ic0 = org_fld%istack_component(j-1)
!
              if(ifield_2_copy(j) .gt. 0 ) then
                i = ifield_2_copy(j)
                ic = merged_fld%istack_component(i-1)
                do nd = 1, org_fld%num_component(j)
                  merged_fld%d_fld(inod_gl,ic+nd)                       &
     &                     = ucd%d_ucd(inod,ic0+nd)
                end do
              end if
            end do
          end if
!
        else if(ioverlap_n(inod_gl) .eq. 0 ) then
          write(*,*) ' ioverlap error !! stop !'
          write(*,*) ' ip, inode ', ip, inod_gl, inod
          stop
        endif
!
      end do
!
      end subroutine copy_udt_field_data_merge
!
! -----------------------------------------------------------------------
!
      end module set_read_geometry_2_merge
