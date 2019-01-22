!set_import_items.f90
!     module set_import_items
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine count_node_import_item(nod_d_grp, ip, num_neib,      &
!!     &          id_neib, ntot_import, istack_import)
!!      subroutine set_node_import_item(nod_d_grp, ip, num_neib,        &
!!     &          id_neib, ntot_import, istack_import, item_import)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!
!!      subroutine count_ele_import_item(ip, nproc, ntot_subd,          &
!!     &          istack_subd, item_subd, num, id_gl_org, IGROUP,       &
!!     &          num_neib, id_neib, ntot_import, istack_import)
!!      subroutine set_ele_import_item(ip, nproc, ntot_subd,            &
!!     &          istack_subd, item_subd, num, id_gl_org, IGROUP,       &
!!     &          num_neib, id_neib, ntot_import, istack_import,        &
!!     &          item_import)
!
      module set_import_items
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine count_node_import_item(nod_d_grp, ip, num_neib,        &
     &          id_neib, ntot_import, istack_import)
!
      use t_domain_group_4_partition
      use m_internal_4_partitioner
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      integer(kind = kint), intent(in) :: ip, num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(inout) :: ntot_import
      integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
!
      integer(kind= kint) :: j, jp, ist, ied, inum, inod, icou
      integer(kind= kint_gl) :: jnod_org
!
!
!
      istack_import(0)= 0
      do j = 1, num_neib
        icou = 0
        jp =  id_neib(j)
        ist = itl_nod_part%istack_4_subdomain(ip-1)                     &
     &       + itl_nod_part%num_inter_sub(ip) + 1
        ied = itl_nod_part%istack_4_subdomain(ip)
        do inum = ist, ied
          inod = itl_nod_part%id_4_subdomain(inum)
          jnod_org = nod_d_grp%id_global_org(inod)
          if (nod_d_grp%IGROUP(jnod_org).eq.jp) icou = icou + 1
        end do
        istack_import(j) = istack_import(j-1) + icou
      end do
      ntot_import = istack_import(num_neib)
!
      end subroutine count_node_import_item
!
!   --------------------------------------------------------------------
!
      subroutine set_node_import_item(nod_d_grp, ip, num_neib,          &
     &          id_neib, ntot_import, istack_import, item_import)
!
      use t_domain_group_4_partition
      use m_internal_4_partitioner
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      integer(kind = kint), intent(in) :: ip, num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
!
      integer(kind= kint) :: j, jp, ist, ied, inum, inod, icou
      integer(kind= kint_gl) :: jnod_org
!
!
      do j = 1, num_neib
        icou = istack_import(j-1)
        jp =  id_neib(j)
        ist = itl_nod_part%istack_4_subdomain(ip-1)                     &
     &       + itl_nod_part%num_inter_sub(ip) + 1
        ied = itl_nod_part%istack_4_subdomain(ip)
        do inum = ist, ied
          inod = itl_nod_part%id_4_subdomain(inum)
          jnod_org = nod_d_grp%id_global_org(inod)
          if (nod_d_grp%IGROUP(jnod_org).eq.jp) then
            icou = icou + 1
            item_import(icou)                                           &
     &           = inum - itl_nod_part%istack_4_subdomain(ip-1)
          end if
        end do
      end do
!
      end subroutine set_node_import_item
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_ele_import_item(ip, nproc, ntot_subd,            &
     &          istack_subd, item_subd, num, id_gl_org, IGROUP,         &
     &          num_neib, id_neib, ntot_import, istack_import)
!
      integer(kind = kint), intent(in) :: ip, nproc
      integer(kind = kint), intent(in) :: istack_subd(0:nproc)
      integer(kind = kint), intent(in) :: ntot_subd
      integer(kind = kint), intent(in) :: item_subd(ntot_subd)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: id_gl_org(num)
      integer(kind = kint), intent(in) :: IGROUP(num)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(inout) :: ntot_import
      integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
!
      integer(kind= kint) :: j, jp, ist, ied, inum, id, icou
      integer(kind= kint_gl) :: jd_org
!
!
!
      istack_import(0)= 0
      do j = 1, num_neib
        icou = 0
        jp =  id_neib(j)
        ist = istack_subd(ip-1) + 1
        ied = istack_subd(ip)
        do inum = ist, ied
          id = item_subd(inum)
          jd_org = id_gl_org(id)
          if (ip.ne.jp) then
            if (IGROUP(jd_org).eq.jp) icou = icou + 1
          else
            if(IGROUP(id).eq.0 .and. IGROUP(jd_org).eq.jp)              &
     &                                icou = icou + 1
          end if
        end do
        istack_import(j) = istack_import(j-1) + icou
      end do
      ntot_import = istack_import(num_neib)
!
      end subroutine count_ele_import_item
!
!------------------------------------------------------------------
!
      subroutine set_ele_import_item(ip, nproc, ntot_subd,              &
     &          istack_subd, item_subd, num, id_gl_org, IGROUP,         &
     &          num_neib, id_neib, ntot_import, istack_import,          &
     &          item_import)
!
      integer(kind = kint), intent(in) :: ip, nproc
      integer(kind = kint), intent(in) :: istack_subd(0:nproc)
      integer(kind = kint), intent(in) :: ntot_subd
      integer(kind = kint), intent(in) :: item_subd(ntot_subd)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(in) :: id_gl_org(num)
      integer(kind = kint), intent(in) :: IGROUP(num)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: ntot_import
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
!
      integer(kind= kint) :: j, jp, ist, ied, inum, id, icou
      integer(kind= kint_gl) :: jd_org
!
!
!
      do j = 1, num_neib
        icou = istack_import(j-1)
        jp =  id_neib(j)
        ist = istack_subd(ip-1) + 1
        ied = istack_subd(ip)
        do inum = ist, ied
          id = item_subd(inum)
          jd_org = id_gl_org(id)
          if (ip.ne.jp) then
            if (IGROUP(jd_org).eq.jp) then
              icou = icou + 1
              item_import(icou) = inum - istack_subd(ip-1)
            end if
          else
            if(IGROUP(id).eq.0 .and. IGROUP(jd_org).eq.jp) then
              icou = icou + 1
              item_import(icou) = inum - istack_subd(ip-1)
            end if
          end if
        end do
      end do
!
      end subroutine set_ele_import_item
!
!------------------------------------------------------------------
!
      end module set_import_items
