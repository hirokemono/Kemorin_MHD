!>@file  set_and_cal_udt_data.f90
!!       module set_and_cal_udt_data
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui on July, 2006
!!@n           Modified by H.Matsui on July, 2013
!
!>@brief set mesh and field data from UCD data input
!!
!!@verbatim
!!      subroutine set_udt_local_nodes(numnod, xx)
!!
!!      subroutine count_udt_elements(internal_node, nele, nnod_ele, ie)
!!      subroutine set_udt_local_connect(internal_node,                 &
!!     &          nele, nnod_ele, ie)
!!      subroutine set_udt_global_connect(internal_node,                &
!!     &          nele, nnod_ele, iele_global, ie)
!!
!!      subroutine set_one_field_to_udt_data(nnod, numdir, i_field,     &
!!     &          d_nod)
!!      subroutine set_one_field_by_udt_data(nnod, numdir, i_field,     &
!!     &          d_nod)
!!
!!      subroutine set_field_by_udt_data(nnod, num_fld, ntot_cmp,       &
!!     &          num_comp, phys_name, d_nod)
!!      subroutine add_field_by_udt_data(nnod, num_fld, ntot_cmp,       &
!!     &          num_comp, phys_name, d_nod)
!!      subroutine subtract_field_by_udt_data(nnod, num_fld, ntot_cmp,  &
!!     &          num_comp, phys_name, d_nod)
!!@endverbatim
!
      module set_and_cal_udt_data
!
      use m_precision
      use m_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_udt_local_nodes(numnod, xx)
!
      integer(kind=kint), intent(in)  :: numnod
      real(kind=kreal), intent(in)  :: xx(numnod,3)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, numnod
        fem_ucd%inod_global(inod) = inod
        fem_ucd%xx(inod,1) = xx(inod,1)
        fem_ucd%xx(inod,2) = xx(inod,2)
        fem_ucd%xx(inod,3) = xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine set_udt_local_nodes
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_udt_elements(internal_node, nele, nnod_ele, ie)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      integer(kind = kint) :: iele, icou
!
!
      fem_ucd%nnod_4_ele = nnod_ele
!
      icou = 0
!$omp parallel do reduction(+:icou)
      do iele = 1, nele
        if(ie(iele,1) .le. internal_node) icou = icou + 1
      end do
!$omp end parallel do
      fem_ucd%nele = icou
!
      end subroutine count_udt_elements
!
!-----------------------------------------------------------------------
!
      subroutine set_udt_local_connect(internal_node,                   &
     &          nele, nnod_ele, ie)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      integer(kind = kint) :: iele, k1, icou
!
!
      icou = 0
      do iele = 1, nele
        if(ie(iele,1) .le. internal_node) then
          icou = icou + 1
          fem_ucd%iele_global(icou) = icou
          do k1 = 1, nnod_ele
            fem_ucd%ie(icou,k1) = ie(iele,k1)
          end do
        end if
      end do
!
      end subroutine set_udt_local_connect
!
!-----------------------------------------------------------------------
!
      subroutine set_udt_global_connect(internal_node,                  &
     &          nele, nnod_ele, iele_global, ie)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint), intent(in)  :: iele_global(nele)
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      integer(kind = kint) :: iele, k1, inod, icou
!
!
      call count_udt_elements(internal_node, nele, nnod_ele, ie)
      call allocate_ucd_ele(fem_ucd)
!
      icou = 0
      do iele = 1, nele
        if(ie(iele,1) .le. internal_node) then
          icou = icou + 1
          fem_ucd%iele_global(icou) = iele_global(iele)
          do k1 = 1, nnod_ele
            inod = ie(iele,k1)
            fem_ucd%ie(icou,k1) = fem_ucd%inod_global(inod)
          end do
        end if
      end do
!
      end subroutine set_udt_global_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_one_field_to_udt_data(nnod, numdir, i_field,       &
     &          d_nod)
!
      integer(kind=kint), intent(in)  :: nnod, i_field, numdir
      real(kind = kreal), intent(inout) :: d_nod(nnod,numdir)
!
      integer(kind = kint) :: inod, nd
!
!
      do nd = 1, numdir
!$omp parallel do
       do inod = 1, nnod
         fem_ucd%d_ucd(inod,nd+i_field-1) = d_nod(inod,nd)
       end do
!$omp end parallel do
      end do
!
      end subroutine set_one_field_to_udt_data
!
! -----------------------------------------------------------------------
!
      subroutine set_one_field_by_udt_data(nnod, numdir, i_field,       &
     &          d_nod)
!
      integer(kind=kint), intent(in)  :: nnod, i_field, numdir
      real(kind = kreal), intent(inout) :: d_nod(nnod,numdir)
!
      integer(kind = kint) :: inod, nd
!
!
      do nd = 1, numdir
!$omp parallel do
       do inod = 1, nnod
         d_nod(inod,nd) = fem_ucd%d_ucd(inod,nd+i_field-1)
       end do
!$omp end parallel do
      end do
!
      end subroutine set_one_field_by_udt_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_field_by_udt_data(nnod, num_fld, ntot_cmp,         &
     &          num_comp, phys_name, d_nod)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: num_comp(num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: i, j, icomp, jcomp
!
!
      icomp = 0
      do i = 1, fem_ucd%num_field
        jcomp = 0
        do j = 1, num_fld
          if ( fem_ucd%phys_name(i) .eq. phys_name(j) ) then
            do nd = 1, num_comp(j)
!$omp parallel do
              do inod = 1, nnod
                d_nod(inod,jcomp+nd) = fem_ucd%d_ucd(inod,icomp+nd)
              end do
!$omp end parallel do
            end do
            exit
          end if
          jcomp = jcomp + num_comp(j)
        end do
        icomp = icomp + fem_ucd%num_comp(i)
      end do
!
      end subroutine set_field_by_udt_data
!
! -----------------------------------------------------------------------
!
      subroutine add_field_by_udt_data(nnod, num_fld, ntot_cmp,         &
     &          num_comp, phys_name, d_nod)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: num_comp(num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: i, j, icomp, jcomp
!
!
      icomp = 0
      do i = 1, fem_ucd%num_field
        jcomp = 0
        do j = 1, num_fld
          if ( fem_ucd%phys_name(i) .eq. phys_name(j) ) then
            do nd = 1, num_comp(j)
!$omp parallel do
              do inod = 1, nnod
                d_nod(inod,jcomp+nd) = d_nod(inod,jcomp+nd)             &
     &                                + fem_ucd%d_ucd(inod,icomp+nd)
              end do
!$omp end parallel do
            end do
          end if
          jcomp = jcomp + num_comp(j)
        end do
        icomp = icomp + fem_ucd%num_comp(i)
      end do
!
      end subroutine add_field_by_udt_data
!
! -----------------------------------------------------------------------
!
      subroutine subtract_field_by_udt_data(nnod, num_fld, ntot_cmp,    &
     &          num_comp, phys_name, d_nod)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: num_comp(num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: i, j, icomp, jcomp
!
!
      icomp = 0
      do i = 1, fem_ucd%num_field
        jcomp = 0
        do j = 1, num_fld
          if ( fem_ucd%phys_name(i) .eq. phys_name(j) ) then
            do nd = 1, num_comp(j)
!$omp parallel do
              do inod = 1, nnod
                d_nod(inod,jcomp+nd) = d_nod(inod,jcomp+nd)             &
     &                                - fem_ucd%d_ucd(inod,icomp+nd)
              end do
!$omp end parallel do
            end do
          end if
          jcomp = jcomp + num_comp(j)
        end do
        icomp = icomp + fem_ucd%num_comp(i)
      end do
!
      end subroutine subtract_field_by_udt_data
!
! -----------------------------------------------------------------------
!
      end module set_and_cal_udt_data
