!>@file   set_element_comm_table.f90
!!@brief  module set_element_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2021
!
!>@brief  Routines to SEt communication table items for elements
!!
!!@verbatim
!!      subroutine count_element_import_num(num_neib, id_neib,          &
!!     &          num_neib_e, id_neib_e, num_import_e, istack_import_e, &
!!     &          ntot_import_e, numele, ip_ref)
!!      subroutine set_element_import_item(numnod, inod_lc, ip_nod,     &
!!     &          numele, nnod_4_ele, ie, x_ele, ip_ref, k_ref,         &
!!     &          num_neib_e, id_neib_e, istack_import_e, item_import_e,&
!!     &          inod_lc_import, ipe_lc_import, xe_import)
!!
!!      subroutine set_element_export_item                              &
!!     &         (txt, numnod, numele, nnod_4_ele, x_ele,               &
!!     &          iele_stack_4_node, iele_4_node,                       &
!!     &          num_neib_e, istack_export_e,                          &
!!     &          inod_lc_export, ipe_lc_export, xe_export,             &
!!     &          item_export_e, fail_tbl)
!!        type(failed_table), intent(inout) :: fail_tbl
!!@endverbatim
!!
      module set_element_comm_table
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_element_import_num(num_neib, id_neib,            &
     &          num_neib_e, id_neib_e, num_import_e, istack_import_e,   &
     &          ntot_import_e, numele, ip_ref)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ip_ref(numele)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(inout) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(inout) :: ntot_import_e
      integer(kind = kint), intent(inout) :: num_import_e(num_neib_e)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint) :: ip, inum, iele
      integer(kind = kint), allocatable :: num_import_tmp(:)
!
!
      allocate(num_import_tmp(nprocs))
!
!$omp parallel workshare
      num_import_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
      do iele = 1, numele
        ip = ip_ref(iele)
        if(ip .ne. my_rank) then
          num_import_tmp(ip+1) = num_import_tmp(ip+1) + 1
        end if
      end do
!
      istack_import_e(0) = 0
      do inum = 1, num_neib
        ip = id_neib(inum)
        id_neib_e(inum) =    ip
        num_import_e(inum) = num_import_tmp(ip+1)
        istack_import_e(inum) = istack_import_e(inum-1)                 &
     &                         + num_import_e(inum)
      end do
      ntot_import_e = istack_import_e(num_neib)
!
      deallocate(num_import_tmp)
!
      end subroutine count_element_import_num
!
!-----------------------------------------------------------------------
!
      subroutine set_element_import_item(numnod, inod_lc, ip_nod,       &
     &          numele, nnod_4_ele, ie, x_ele, ip_ref, k_ref,           &
     &          num_neib_e, id_neib_e, istack_import_e, item_import_e,  &
     &          inod_lc_import, ipe_lc_import, xe_import)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: inod_lc(numnod)
      integer(kind = kint), intent(in) :: ip_nod(numnod)
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: ip_ref(numele)
      integer(kind = kint), intent(in) :: k_ref(numele)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_import_e(istack_import_e(num_neib_e))
      real(kind = kreal), intent(inout)                                 &
     &        :: xe_import(3*istack_import_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint) :: ip, icou, iele
      integer(kind = kint) :: ist, inum, inod
      integer(kind = kint) :: k1
!
      integer(kind = kint), allocatable :: ip_rev_tmp(:)
      integer(kind = kint), allocatable :: num_import_tmp(:)
!
!
      allocate(ip_rev_tmp(nprocs))
      allocate(num_import_tmp(nprocs))
!
!$omp parallel workshare
      ip_rev_tmp(1:nprocs) =     0
      num_import_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(ip)
      do inum = 1, num_neib_e
        ip = id_neib_e(inum)
        ip_rev_tmp(ip+1) = inum
      end do
!$omp end parallel do
!
      do iele = 1, numele
        ip =   ip_ref(iele)
        if(ip .ne. my_rank) then
          inum = ip_rev_tmp(ip+1)
          num_import_tmp(ip+1) = num_import_tmp(ip+1) + 1
          icou = istack_import_e(inum-1) + num_import_tmp(ip+1)
!
          item_import_e(icou) = iele
        end if
      end do
!
      deallocate(ip_rev_tmp, num_import_tmp)
!
      do icou = 1, istack_import_e(num_neib_e)
        iele = item_import_e(icou)
!
        xe_import(3*icou-2) = x_ele(iele,1)
        xe_import(3*icou-1) = x_ele(iele,2)
        xe_import(3*icou  ) = x_ele(iele,3)
        do k1 = 1, nnod_4_ele
          inod = ie(iele,k1)
          inod_lc_import(icou,k1) = inod_lc(inod)
          ipe_lc_import(icou,k1) =  ip_nod(inod)
        end do
      end do
!
      end subroutine  set_element_import_item
!
!-----------------------------------------------------------------------
!
      subroutine set_element_export_item                                &
     &         (txt, numnod, numele, nnod_4_ele, x_ele,                 &
     &          iele_stack_4_node, iele_4_node,                         &
     &          num_neib_e, istack_export_e,                            &
     &          inod_lc_export, ipe_lc_export, xe_export,               &
     &          item_export_e, fail_tbl)
!
      use t_failed_export_list
      use calypso_mpi_int
      use quicksort
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_4_node(iele_stack_4_node(numnod))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      real(kind = kreal), intent(in)                                    &
     &        :: xe_export(3*istack_export_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_export_e(istack_export_e(num_neib_e))
      type(failed_table), intent(inout) :: fail_tbl
!
      integer(kind = kint) :: ip, iflag, icou, num_gl
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, jed, jnum, jele
      integer(kind = kint) :: k1, kk
      real(kind = kreal) :: dist, dist_min
!
      integer(kind = kint) :: inod_sf_lc
      integer(kind = kint) :: n_search(nnod_4_ele)
      integer(kind = kint) :: idx_sort(nnod_4_ele)
      type(failed_item) :: fail_comm_t
!
!
      icou = 0
      do ip = 1, num_neib_e
        ist = istack_export_e(ip-1) + 1
        ied = istack_export_e(ip)
        do inum = ist, ied
          do k1 = 1, nnod_4_ele
            idx_sort(k1) = k1
            inod_sf_lc = inod_lc_export(inum,k1)
            if(ipe_lc_export(inum,k1) .eq. my_rank) then
              n_search(k1) = iele_stack_4_node(inod_sf_lc)              &
     &                      - iele_stack_4_node(inod_sf_lc-1)
            else
              n_search(k1) = 0
            end if
          end do
          call quicksort_w_index                                        &
     &       (nnod_4_ele, n_search, ione, nnod_4_ele, idx_sort)
!
          iflag = 0
          dist_min = 1.0d30
          do k1 = 1, nnod_4_ele
            kk = idx_sort(k1)
            if(ipe_lc_export(inum,kk) .ne. my_rank) cycle
!
            inod = inod_lc_export(inum,kk)
            jst = iele_stack_4_node(inod-1) + 1
            jed = iele_stack_4_node(inod)
            do jnum = jst, jed
              jele = iele_4_node(jnum)
!
              dist = sqrt((x_ele(jele,1)- xe_export(3*inum-2))**2 &
     &                + (x_ele(jele,2) - xe_export(3*inum-1))**2  &
     &                + (x_ele(jele,3) - xe_export(3*inum  ))**2)
!
              if(dist .le. TINY) then
                item_export_e(inum) = jele
                iflag = 1
                exit
              end if
              dist_min = min(dist_min,dist)
            end do
            if(iflag .eq. 1) exit
          end do
          if(iflag .eq. 0) then
            icou = icou + 1
            call set_failed_export(inum, item_export_e(inum), dist_min, &
     &                             fail_comm_t)
            call append_failed_export(fail_comm_t, fail_tbl)
          end if
        end do
      end do
!
      call calypso_mpi_barrier
      call calypso_mpi_allreduce_one_int(icou, num_gl, MPI_SUM)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &   'Failed export by set_element_export_item', num_gl
!
      end subroutine set_element_export_item
!
!-----------------------------------------------------------------------
!
      end module set_element_comm_table
