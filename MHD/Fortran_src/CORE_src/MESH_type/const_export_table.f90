!const_export_table.f90
!      module const_export_table
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine count_ele_comm_neib(my_rank, nprocs, num_neib)
!        integer(kind = kint), intent(in) :: my_rank, nprocs
!        integer(kind = kint), intent(inout) :: num_neib
!
!      subroutine set_ele_comm_neib(my_rank, nprocs, num_neib, id_neib)
!        integer(kind = kint), intent(in) :: my_rank, nprocs
!        integer(kind = kint), intent(in) :: num_neib
!        integer(kind = kint), intent(inout) :: id_neib(num_neib)
!
!      subroutine set_ele_comm_tbl_num(my_rank, num_neib, id_neib,      &
!     &          ntot_import, ntot_export, num_import, num_export,      &
!     &          istack_import, istack_export)
!        integer(kind = kint), intent(in) :: my_rank
!        integer(kind = kint), intent(in) :: num_neib
!        integer(kind = kint), intent(in) :: id_neib(num_neib)
!
!        integer(kind = kint), intent(inout) :: ntot_import, ntot_export
!        integer(kind = kint), intent(inout) :: num_import(num_neib)
!        integer(kind = kint), intent(inout) :: num_export(num_neib)
!        integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
!        integer(kind = kint), intent(inout) :: istack_export(0:num_neib)
!
!      subroutine set_ele_import_item(my_rank, num_neib, ntot_import,   &
!     &          id_neib, num_import, istack_import, item_import)
!        integer(kind = kint), intent(in) :: my_rank
!        integer(kind = kint), intent(in) :: num_neib, ntot_import
!        integer(kind = kint), intent(in) :: id_neib(num_neib)
!        integer(kind = kint), intent(in) :: num_import(num_neib)
!        integer(kind = kint), intent(in) :: istack_import(0:num_neib)
!
!        integer(kind = kint), intent(inout) :: item_import(ntot_import)
!
!      subroutine set_ele_export_item(numnod, internal_node,            &
!     &          numele, nnod_4_ele, id_global, ie, num_neib, id_neib,  &
!     &          ntot_export, num_export, istack_export, item_export)
!        integer(kind = kint), intent(in) :: numnod, internal_node
!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!        integer(kind = kint), intent(in) :: id_global(numnod)
!        integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!        integer(kind = kint), intent(in) :: num_neib, ntot_export
!        integer(kind = kint), intent(in) :: id_neib(num_neib)
!        integer(kind = kint), intent(in) :: num_export(num_neib)
!        integer(kind = kint), intent(in) :: istack_export(0:num_neib)
!
!        integer(kind = kint), intent(inout) :: item_export(ntot_export)
!
      module const_export_table
!
      use m_precision
!
      use m_const_ele_comm_tbl
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_ele_comm_neib(my_rank, nprocs, num_neib)
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      integer(kind = kint), intent(inout) :: num_neib
!
      integer(kind = kint) :: ip, j
!
!
      num_neib = 0
      do ip = 1, nprocs
        do j = 1, ele_comm_tmp(ip)%num_neib
          if(ele_comm_tmp(ip)%id_neib(j) .eq. my_rank) then
            num_neib = num_neib + 1
          end if
        end do
      end do
!
      end subroutine count_ele_comm_neib
!
!------------------------------------------------------------------
!
      subroutine set_ele_comm_neib(my_rank, nprocs, num_neib, id_neib)
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(inout) :: id_neib(num_neib)
!
      integer(kind = kint) :: ip, i, j
!
!
      i = 0
      do ip = 1, nprocs
        do j = 1, ele_comm_tmp(ip)%num_neib
          if(ele_comm_tmp(ip)%id_neib(j) .eq. my_rank) then
            i = i + 1
            id_neib(i) = ip - 1
          end if
        end do
      end do
!
      end subroutine set_ele_comm_neib
!
!------------------------------------------------------------------
!
      subroutine set_ele_comm_tbl_num(my_rank, num_neib, id_neib,       &
     &          ntot_import, ntot_export, num_import, num_export,       &
     &          istack_import, istack_export)
!
      integer(kind = kint), intent(in) :: my_rank
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(inout) :: ntot_import, ntot_export
      integer(kind = kint), intent(inout) :: num_import(num_neib)
      integer(kind = kint), intent(inout) :: num_export(num_neib)
      integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
      integer(kind = kint), intent(inout) :: istack_export(0:num_neib)
!
      integer(kind = kint) :: i, ip, j
!
!
      ip = my_rank + 1
      do i = 1, num_neib
        do j = 1, ele_comm_tmp(ip)%num_neib
          if(ele_comm_tmp(ip)%id_neib(j) .eq. id_neib(i)) then
            num_import(i) = ele_comm_tmp(ip)%num_import(j)
            exit
          end if
        end do
      end do
!
      do i = 1, num_neib
        ip = id_neib(i) + 1
        do j = 1, ele_comm_tmp(ip)%num_neib
          if(ele_comm_tmp(ip)%id_neib(j) .eq. (ip-1) ) then
            num_export(i) = ele_comm_tmp(ip)%num_import(j)
            exit
          end if
        end do
      end do
!
      istack_import(0) = 0
      istack_export(0) = 0
      do i = 1, num_neib
        istack_import(i) = istack_import(i-1) + num_import(i)
        istack_export(i) = istack_export(i-1) + num_export(i)
      end do
      ntot_import = istack_import(num_neib)
      ntot_export = istack_export(num_neib)
!
!
      end subroutine set_ele_comm_tbl_num
!
!------------------------------------------------------------------
!
      subroutine set_ele_import_item(my_rank, num_neib, ntot_import,    &
     &          id_neib, num_import, istack_import, item_import)
!
      integer(kind = kint), intent(in) :: my_rank
!
      integer(kind = kint), intent(in) :: num_neib, ntot_import
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: num_import(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
!
      integer(kind = kint) :: inum, i, ip, j, ii, jj
!
!
      ip = my_rank + 1
      do i = 1, num_neib
        do j = 1, ele_comm_tmp(ip)%num_neib
          if(ele_comm_tmp(ip)%id_neib(j) .eq. id_neib(i)) then
            do inum = 1, num_import(i)
              ii = inum + istack_import(i-1)
              jj = inum + ele_comm_tmp(ip)%istack_import(j-1)
              item_import(ii) = ele_comm_tmp(ip)%item_import(jj)
            end do
            exit
          end if
        end do
      end do
!
      end subroutine set_ele_import_item
!
!------------------------------------------------------------------
!
      subroutine set_ele_export_item(numnod, internal_node,             &
     &          numele, nnod_4_ele, id_global, ie, num_neib, id_neib,   &
     &          ntot_export, num_export, istack_export, item_export)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: id_global(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_export
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: num_export(num_neib)
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
!
      integer(kind = kint), intent(inout) :: item_export(ntot_export)
!
      integer(kind = kint) :: inod, iele, k1, i, ip, j, ii, jj
      integer(kind = kint) :: inum, iflag
      integer(kind = kint) :: ie_ref(nnod_4_ele)
!
!
      do iele = 1, numele
        if( ie(iele,1) .le. internal_node) then
          do k1 = 1, nnod_4_ele
            inod = ie(iele,k1)
            ie_ref(k1) = id_global(inod)
          end do
!
          do i = 1, num_neib
            ip = id_neib(i) + 1
            do j = 1, ele_comm_tmp(ip)%num_neib
              if(ele_comm_tmp(ip)%id_neib(j) .eq. (ip-1)) then
                do inum = 1, num_export(i)
                  ii = inum + istack_export(i-1)
                  jj = inum + ele_comm_tmp(ip)%istack_import(j-1)
                  if( item_export(ii) .eq. 0) then
!
                    iflag = 1
                    do k1 = 1, nnod_4_ele
                      if(ele_comm_work(ip)%ie_gl_import(k1,jj)          &
     &                 .ne. ie_ref(k1) ) then
                        iflag = 0
                        exit
                      end if
                    end do
!
                    if(iflag .eq. 1) item_export(ii) = iele
!
                  end if
                end do
              end if
            end do
          end do
!
        end if
      end do
!
      end subroutine set_ele_export_item
!
!------------------------------------------------------------------
!
      end module const_export_table
