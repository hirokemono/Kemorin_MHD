!> @file  const_import_table.f90
!!      module const_import_table
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2008
!
!> @brief Routines to make import table
!!
!!@verbatim
!!      subroutine count_import_domain(nprocs, num, id_org_domain,      &
!!     &          num_neib)
!!        integer(kind = kint), intent(inout) :: num_neib
!!      subroutine set_import_domain(nprocs, num, id_org_domain,        &
!!     &          num_neib, id_neib)
!!        integer(kind = kint), intent(inout) :: id_neib(num_neib)
!!      subroutine count_num_import(num, id_org_domain, num_neib,       &
!!     &          id_neib, num_import, istack_import, ntot_import)
!!        integer(kind = kint), intent(inout) :: ntot_import
!!        integer(kind = kint), intent(inout) :: num_import(num_neib)
!!        integer(kind = kint), intent(inout) ::istack_import(0:num_neib)
!!      subroutine set_import_item(num, id_org_domain, num_neib,        &
!!     &          id_neib, istack_import, ntot_import, item_import)
!!        integer(kind = kint), intent(inout) :: item_import(ntot_import)
!!@endverbatim
!
      module const_import_table
!
      use m_precision
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_import_domain(nprocs, num, id_org_domain,        &
     &          num_neib)
!
      integer(kind = kint), intent(in) :: nprocs, num
      integer(kind = kint), intent(in) :: id_org_domain(num)
!
      integer(kind = kint), intent(inout) :: num_neib
!
      integer(kind = kint) :: ip, id_proc, ncou, i
!
!
      do ip = 1, nprocs
        id_proc = ip - 1
        ncou = 0
        do i = 1, num
          if(id_org_domain(num) .eq. id_proc) then
            ncou = ncou + 1
            exit
          end if
        end do
!
        if(ncou .gt. 0) num_neib = num_neib + 1
      end do
!
      end subroutine count_import_domain
!
!------------------------------------------------------------------
!
      subroutine set_import_domain(nprocs, num, id_org_domain,          &
     &          num_neib, id_neib)
!
      integer(kind = kint), intent(in) :: nprocs, num
      integer(kind = kint), intent(in) :: id_org_domain(num)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(inout) :: id_neib(num_neib)
!
      integer(kind = kint) :: ip, id_proc, icou, ncou, i
!
!
      icou = 0
      do ip = 1, nprocs
        id_proc = ip - 1
        ncou = 0
        do i = 1, num
          if(id_org_domain(num) .eq. id_proc) then
            ncou = ncou + 1
            exit
          end if
        end do
!
        if(ncou .gt. 0) then
          icou = icou + 1
          id_neib(icou) = id_proc
        end if
      end do
!
      end subroutine set_import_domain
!
!------------------------------------------------------------------
!
      subroutine count_num_import(num, id_org_domain, num_neib,         &
     &          id_neib, num_import, istack_import, ntot_import)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: id_org_domain(num)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(inout) :: ntot_import
      integer(kind = kint), intent(inout) :: num_import(num_neib)
      integer(kind = kint), intent(inout) :: istack_import(0:num_neib)
!
      integer(kind = kint) :: ip, id_proc, i
!
      num_import(1:num_neib) =    0
      istack_import(0:num_neib) = 0
      do ip = 1, num_neib
        id_proc = id_neib(ip)
        do i = 1, num
          if(id_org_domain(num) .eq. id_proc) then
            num_import(ip) = num_import(ip) + 1
          end if
        end do
!
        istack_import(ip) = istack_import(ip-1) + num_import(ip)
      end do
      ntot_import = istack_import(num_neib)
!
      end subroutine count_num_import
!
!------------------------------------------------------------------
!
      subroutine set_import_item(num, id_org_domain, num_neib,          &
     &          id_neib, istack_import, ntot_import, item_import)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: id_org_domain(num)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_import
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
!
      integer(kind = kint), intent(inout) :: item_import(ntot_import)
!
      integer(kind = kint) :: ip, id_proc, icou, i
!
!
      do ip = 1, num_neib
        id_proc = id_neib(ip)
        icou = istack_import(ip-1)
        do i = 1, num
          if(id_org_domain(num) .eq. id_proc) then
            icou = icou + 1
            item_import(icou) = i
          end if
        end do
      end do
!
      end subroutine set_import_item
!
!------------------------------------------------------------------
!
      end module const_import_table
