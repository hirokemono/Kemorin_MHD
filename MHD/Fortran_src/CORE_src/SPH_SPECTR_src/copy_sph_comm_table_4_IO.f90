!>@file   copy_sph_comm_table_4_IO.f90
!!@brief  module copy_sph_comm_table_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007 
!
!>@brief  Copy communication table for spherical hermonica transform
!!        between IO data
!!
!!@verbatim
!!      subroutine copy_comm_sph_from_IO(numnod, comm, comm_sph)
!!        type(communication_table), intent(in) :: comm
!!        type(sph_comm_tbl), intent(inout) :: comm_sph
!!      subroutine copy_comm_sph_to_comm_tbl(comm_sph, comm)
!!        type(sph_comm_tbl), intent(in) :: comm_sph
!!        type(communication_table), intent(inout) :: comm
!!@endverbatim
!!
!!@n @param nnod_rtp
!!      number of data points for @f$ f(r,\theta,\phi) @f$
!!@n @param nnod_rtm
!!      number of data points for @f$ f(r,\theta,m) @f$
!!@n @param nnod_rlm 
!!      number of data points for @f$ f(r,l,m) @f$
!!@n @param nnod_rj  
!!      number of data points for @f$ f(r,j) @f$
!!
!
      module copy_sph_comm_table_4_IO
!
      use m_precision
!
      use m_constants
!
      use t_comm_table
      use t_sph_trans_comm_tbl
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_sph_from_IO(numnod, comm, comm_sph)
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: comm
      type(sph_comm_tbl), intent(inout) :: comm_sph
!
!
      comm_sph%nneib_domain = comm%num_neib
      comm_sph%ntot_item_sr = comm%ntot_import
!
      call alloc_sph_comm_stack(comm_sph)
      call alloc_sph_comm_item(numnod, comm_sph)
!
      comm_sph%id_domain(1:comm_sph%nneib_domain)                       &
     &      = comm%id_neib(1:comm_sph%nneib_domain)
      comm_sph%istack_sr(0:comm_sph%nneib_domain)                       &
     &      = comm%istack_import(0:comm_sph%nneib_domain)
!
      comm_sph%item_sr(1:comm_sph%ntot_item_sr)                         &
     &      = comm%item_import(1:comm_sph%ntot_item_sr)
!
      end subroutine copy_comm_sph_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_sph_to_comm_tbl(comm_sph, comm)
!
      type(sph_comm_tbl), intent(in) :: comm_sph
      type(communication_table), intent(inout) :: comm
!
!
      comm%num_neib =    comm_sph%nneib_domain
      comm%ntot_import = comm_sph%ntot_item_sr
!
      call alloc_neighbouring_id(comm)
      call alloc_import_num(comm)
      call alloc_import_item(comm)
!
      comm%id_neib(1:comm_sph%nneib_domain)                             &
     &      = comm_sph%id_domain(1:comm_sph%nneib_domain)
      comm%istack_import(0:comm_sph%nneib_domain)                       &
     &      = comm_sph%istack_sr(0:comm_sph%nneib_domain)
!
      comm%item_import(1:comm_sph%ntot_item_sr)                         &
     &      = comm_sph%item_sr(1:comm_sph%ntot_item_sr)
!
      end subroutine copy_comm_sph_to_comm_tbl
!
! -----------------------------------------------------------------------

      end module copy_sph_comm_table_4_IO
