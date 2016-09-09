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
!!      subroutine copy_comm_sph_from_IO(numnod, comm_sph)
!!      subroutine copy_comm_sph_to_IO(my_rank, comm_sph)
!!        type(sph_comm_tbl), intent(inout) :: comm_sph
!!@endverbatim
!!
!!@n @param my_rank   running process ID
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
      use m_comm_data_IO
!
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
      subroutine copy_comm_sph_from_IO(numnod, comm_sph)
!
      integer(kind = kint), intent(in) :: numnod
      type(sph_comm_tbl), intent(inout) :: comm_sph
!
!
      comm_sph%nneib_domain = comm_IO%num_neib
      comm_sph%ntot_item_sr = comm_IO%ntot_import
!
      call alloc_type_sph_comm_stack(comm_sph)
      call alloc_type_sph_comm_item(numnod, comm_sph)
!
      comm_sph%id_domain(1:comm_sph%nneib_domain)                       &
     &      = comm_IO%id_neib(1:comm_sph%nneib_domain)
      comm_sph%istack_sr(0:comm_sph%nneib_domain)                       &
     &      = istack_import_IO(0:comm_sph%nneib_domain)
!
      comm_sph%item_sr(1:comm_sph%ntot_item_sr)                         &
     &      = comm_IO%item_import(1:comm_sph%ntot_item_sr)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_sph_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_sph_to_IO(my_rank, comm_sph)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_comm_tbl), intent(inout) :: comm_sph
!
!
      my_rank_IO = my_rank
      comm_IO%num_neib =    comm_sph%nneib_domain
      comm_IO%ntot_import = comm_sph%ntot_item_sr
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      comm_IO%id_neib(1:comm_sph%nneib_domain)                          &
     &      = comm_sph%id_domain(1:comm_sph%nneib_domain)
      istack_import_IO(0:comm_sph%nneib_domain)                         &
     &      = comm_sph%istack_sr(0:comm_sph%nneib_domain)
!
      comm_IO%item_import(1:comm_sph%ntot_item_sr)                      &
     &      = comm_sph%item_sr(1:comm_sph%ntot_item_sr)
!
      call dealloc_type_sph_comm_item(comm_sph)
!
      end subroutine copy_comm_sph_to_IO
!
! -----------------------------------------------------------------------

      end module copy_sph_comm_table_4_IO
