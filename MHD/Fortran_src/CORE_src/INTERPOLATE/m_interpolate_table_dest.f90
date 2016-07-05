!>@file   m_interpolate_table_dest.f90
!!@brief  module m_interpolate_table_dest
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in  Aug., 2006
!
!> @brief Interpolation table for target grid
!!@verbatim
!!      subroutine allocate_itp_num_dest(num_org_pe)
!!      subroutine allocate_itp_table_dest
!!
!!      subroutine deallocate_itp_num_dest
!!      subroutine deallocate_itp_table_dest
!!@endverbatim
!
!
      module m_interpolate_table_dest
!
      use m_precision
      use t_interpolate_tbl_dest
!
      implicit none
!
!> Structure of interpolation table for target grid
      type(interpolate_table_dest), save :: itp1_dest
!itp1_dest%istack_nod_tbl_dest
!
!>   number of subdomain to receive interpolated data
!      integer(kind = kint) :: num_org_domain
!>   flag if original nodes have same prosess
!      integer(kind = kint) :: iflag_self_itp_recv
!>   subdomain rank to receive interpolated data
!      integer(kind = kint), allocatable :: id_org_domain(:)
!>   end address to receive interpolated data
!      integer(kind = kint), allocatable :: istack_nod_tbl_dest(:)
!
!>   total number of interpolated node in target subdomain
      integer(kind = kint) :: ntot_table_dest
!>   local node ID to set interpolated data (import)
      integer(kind = kint), allocatable :: inod_dest_4_dest(:)
!>   Reverse ID to set interpolated data (import)
      integer(kind = kint), allocatable :: irev_dest_4_dest(:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_table_dest
!
      allocate( inod_dest_4_dest(ntot_table_dest) )
      allocate( irev_dest_4_dest(ntot_table_dest) )
!
      if(ntot_table_dest .gt. 0) then
        inod_dest_4_dest = 0
        irev_dest_4_dest = 0
      end if
!
      end subroutine allocate_itp_table_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_table_dest
!
      deallocate( inod_dest_4_dest, irev_dest_4_dest )
!
      end subroutine deallocate_itp_table_dest
!
!-----------------------------------------------------------------------
!
      end module m_interpolate_table_dest
