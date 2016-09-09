!>@file  copy_sph_comm_tbl_type_4_IO.f90
!!      module copy_sph_comm_tbl_type_4_IO
!!
!!@author  H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy communication table for spherical harmonics transform
!!@n     between IO buffer
!!
!!@verbatim
!!      subroutine copy_comm_sph_type_from_IO(my_rank, numnod, comm)
!!      subroutine copy_comm_sph_type_to_IO(my_rank, comm)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_comm_tbl), intent(inout) :: comm
!!@endverbatim
!
      module copy_sph_comm_tbl_type_4_IO
!
      use m_precision
!
      use m_constants
      use t_sph_trans_comm_tbl
      use m_comm_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_sph_type_from_IO(my_rank, numnod, comm)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: numnod
      type(sph_comm_tbl), intent(inout) :: comm
!
!
      comm%nneib_domain = comm_IO%num_neib
      comm%ntot_item_sr = comm_IO%ntot_import
!
      call alloc_type_sph_comm_stack(comm)
      call alloc_type_sph_comm_item(numnod, comm)
!
      comm%id_domain(1:comm%nneib_domain)                               &
     &      = comm_IO%id_neib(1:comm%nneib_domain)
      comm%istack_sr(0:comm%nneib_domain)                               &
     &      = comm_IO%istack_import(0:comm%nneib_domain)
!
      comm%item_sr(1:comm%ntot_item_sr)                                 &
     &      = comm_IO%item_import(1:comm%ntot_item_sr)
!
      call deallocate_type_import(comm_IO)
      call deallocate_type_neib_id(comm_IO)
!
      call set_reverse_sph_comm_tbl_t(numnod, comm)
!
      if(comm%id_domain(comm%nneib_domain) .eq. my_rank) then
        comm%iflag_self = 1
      else
        comm%iflag_self = 0
      end if
!
      end subroutine copy_comm_sph_type_from_IO
!
! -----------------------------------------------------------------------
!
      end module copy_sph_comm_tbl_type_4_IO
