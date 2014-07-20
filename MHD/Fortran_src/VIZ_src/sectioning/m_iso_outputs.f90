!
!      module m_iso_outputs
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine allocate_iso_outputs_num(nprocs, my_rank, num_iso)
!!      subroutine allocate_iso_outputs_data(my_rank, num_iso)
!!      subroutine allocate_SR_array_iso(my_rank, max_ncomp_iso_out,    &
!!     &          nnod_iso_tot, npatch_tot)
!!
!!      subroutine deallocate_iso_outputs_num
!!      subroutine deallocate_iso_outputs_data(my_rank, num_iso)
!!      subroutine deallocate_SR_array_iso(my_rank)
!
      module m_iso_outputs
!
      use m_precision
      use m_geometry_constants
!
      use t_ucd_data
!
      implicit  none
!
!
!>      Structure for isosurface output (used by master process)
      type(ucd_data), allocatable, save :: iso_out(:)
!
      integer(kind = kint) :: ntot_nod_output_iso = 0
      integer(kind = kint), allocatable, target                         &
     &              :: istack_nod_output_iso(:)
!
      integer(kind = kint) :: nmax_nod_para_iso = 0
      integer(kind = kint), allocatable :: nnod_para_iso(:)
      integer(kind = kint), allocatable :: istack_nod_para_iso(:)
!
      integer(kind = kint), allocatable :: nnod_recv_iso(:)
      integer(kind = kint), allocatable :: istack_nod_recv_iso(:)
!
      integer(kind = kint) :: ntot_ele_output_iso = 0
      integer(kind = kint), allocatable, target                         &
     &              :: istack_ele_output_iso(:)
!
      integer(kind = kint) :: nmax_ele_para_iso = 0
      integer(kind = kint), allocatable :: nele_para_iso(:)
      integer(kind = kint), allocatable :: istack_ele_para_iso(:)
!
      integer(kind = kint), allocatable :: nele_recv_iso(:)
      integer(kind = kint), allocatable :: istack_ele_recv_iso(:)
!
      integer(kind = kint), allocatable :: ihash_output_iso(:)
!
!
      real(kind = kreal), allocatable :: send_iso(:)
      real(kind = kreal), allocatable :: recv_iso(:)
      integer(kind = kint), allocatable :: isend_iso(:)
      integer(kind = kint), allocatable :: irecv_iso(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_iso_outputs_num(nprocs, my_rank, num_iso)
!
      integer(kind = kint), intent(in) :: num_iso, nprocs, my_rank
!
!
      if(my_rank .eq. 0) then
        allocate( iso_out(num_iso) )
      else
        allocate( iso_out(0) )
      end if
!
      allocate( istack_nod_output_iso(0:num_iso) )
      allocate( nnod_para_iso(num_iso*nprocs) )
      allocate( istack_nod_para_iso(0:num_iso*nprocs) )
      allocate( nnod_recv_iso(num_iso*nprocs) )
      allocate( istack_nod_recv_iso(0:num_iso*nprocs) )
!
      allocate( istack_ele_output_iso(0:num_iso) )
      allocate( nele_para_iso(num_iso*nprocs) )
      allocate( istack_ele_para_iso(0:num_iso*nprocs) )
      allocate( nele_recv_iso(num_iso*nprocs) )
      allocate( istack_ele_recv_iso(0:num_iso*nprocs) )
!
      nnod_para_iso = 0
      nnod_recv_iso = 0
      istack_nod_output_iso = 0
      istack_nod_para_iso = 0
      istack_nod_recv_iso = 0
!
      nele_para_iso = 0
      nele_recv_iso = 0
      istack_ele_output_iso = 0
      istack_ele_para_iso = 0
      istack_ele_recv_iso = 0
!
      end subroutine allocate_iso_outputs_num
!
! ----------------------------------------------------------------------
!
      subroutine allocate_iso_outputs_data(my_rank, num_iso)
!
      integer(kind=kint ) , intent(in) :: my_rank, num_iso
!
      integer(kind = kint) :: i_iso
!
!
      allocate( ihash_output_iso(ntot_nod_output_iso) )
      if(ntot_nod_output_iso .gt. 0) ihash_output_iso = 0
!
      if(my_rank .eq. 0) then
        do i_iso = 1, num_iso
          call allocate_ucd_node(iso_out(i_iso))
          call allocate_ucd_ele(iso_out(i_iso))
          call allocate_ucd_phys_data(iso_out(i_iso))
        end do
      end if
!
      end subroutine allocate_iso_outputs_data
!
! ----------------------------------------------------------------------
!
      subroutine allocate_SR_array_iso(my_rank, max_ncomp_iso_out,      &
     &          nnod_iso_tot, npatch_tot)
!
      integer(kind=kint ) , intent(in) ::  my_rank
      integer(kind=kint ) , intent(in) ::  max_ncomp_iso_out
      integer(kind = kint), intent(in) :: nnod_iso_tot
      integer(kind = kint), intent(in) :: npatch_tot
!
      integer(kind = kint) :: nmax_comp, nmax_int
!
!
      nmax_comp = max(max_ncomp_iso_out,num_triangle)
      allocate (send_iso(nmax_comp*nnod_iso_tot))
      if(nmax_comp*nnod_iso_tot .gt. 0) send_iso = 0.0d0
!
      nmax_int = max(num_triangle*npatch_tot, nnod_iso_tot)
      allocate (isend_iso(num_triangle*npatch_tot))
      if(nmax_int .gt. 0) isend_iso = 0
!
      if (my_rank.eq.0) then
        allocate (recv_iso(nmax_comp*ntot_nod_output_iso))
        if(nmax_comp*nnod_iso_tot .gt. 0)  recv_iso = 0.0d0
!
        nmax_int                                                        &
     &    = max(num_triangle*ntot_ele_output_iso, ntot_nod_output_iso)
        allocate (irecv_iso(num_triangle*ntot_ele_output_iso))
        if(nmax_int .gt. 0) irecv_iso = 0
      end if
!
      end subroutine allocate_SR_array_iso
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_iso_outputs_num
!
      deallocate( istack_nod_output_iso)
      deallocate( nnod_para_iso)
      deallocate( istack_nod_para_iso)
!
      deallocate( istack_ele_output_iso)
      deallocate( nele_para_iso)
      deallocate( istack_ele_para_iso)
!
      deallocate( iso_out )
!
      end subroutine deallocate_iso_outputs_num
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_iso_outputs_data(my_rank, num_iso)
!
      integer(kind = kint), intent(in) :: my_rank, num_iso
      integer(kind = kint) :: i_iso
!
!
      deallocate(ihash_output_iso )
!
      if(my_rank .eq. 0) then
        do i_iso = 1, num_iso
          call deallocate_ucd_mesh(iso_out(i_iso))
        end do
      end if
!
      end subroutine deallocate_iso_outputs_data
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_SR_array_iso(my_rank)
!
      integer(kind=kint ) , intent(in)   ::  my_rank
!      number of domain
!
        deallocate (send_iso )
        deallocate (isend_iso )
!
      if (my_rank.eq.0) then
        deallocate (recv_iso)
        deallocate (irecv_iso)
      end if
!
      end subroutine deallocate_SR_array_iso
!
! ----------------------------------------------------------------------
!
      end module m_iso_outputs
