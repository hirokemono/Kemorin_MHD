!>@file   m_psf_outputs.f90
!!@brief  module m_psf_outputs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on July, 2006
!!@n       Modified by H.Matsui on March, 2013
!
!>@brief Output data for parallel surface rendering
!!
!!@verbatim
!!      subroutine allocate_psf_outputs_num(nprocs, my_rank, num_psf)
!!      subroutine allocate_psf_outputs_data(my_rank, num_psf)
!!      subroutine allocate_SR_array_psf(my_rank, max_ncomp_psf_out,    &
!!     &          nnod_psf_tot, npatch_tot_psf_smp)
!!
!!      subroutine deallocate_psf_outputs_num
!!      subroutine deallocate_psf_outputs_data(my_rank, num_psf)
!!      subroutine deallocate_SR_array_psf(my_rank)
!!@endverbatim
!!
!!@param my_rank       subdomain ID
!!@param nprocs        number of total processes
!!@param num_psf       number of sections
!!@param max_ncomp_psf_out    maximum number of components for sections
!!@param nnod_psf_tot         total number of node for sections
!!@param npatch_tot_psf_smp   total number of elements for sections
!
      module m_psf_outputs
!
      use m_precision
      use m_geometry_constants
!
      use t_ucd_data
!
      implicit  none
!
!>      Structure for cross sectioning output (used by master process)
      type(ucd_data), allocatable, save :: psf_out(:)
!
      integer(kind = kint) :: ntot_nod_output_psf = 0
      integer(kind = kint), allocatable, target                         &
     &              :: istack_nod_output_psf(:)
!
      integer(kind = kint) :: nmax_nod_para_psf = 0
      integer(kind = kint), allocatable :: nnod_para_psf(:)
      integer(kind = kint), allocatable :: istack_nod_para_psf(:)
!
      integer(kind = kint), allocatable :: nnod_recv_psf(:)
      integer(kind = kint), allocatable :: istack_nod_recv_psf(:)
!
      integer(kind = kint) :: ntot_ele_output_psf = 0
      integer(kind = kint), allocatable, target                         &
     &              :: istack_ele_output_psf(:)
!
      integer(kind = kint) :: nmax_ele_para_psf = 0
      integer(kind = kint), allocatable :: nele_para_psf(:)
      integer(kind = kint), allocatable :: istack_ele_para_psf(:)
!
      integer(kind = kint), allocatable :: nele_recv_psf(:)
      integer(kind = kint), allocatable :: istack_ele_recv_psf(:)
!
      integer(kind = kint), allocatable :: ihash_output_psf(:)
!
!
      real(kind = kreal), allocatable :: send_psf(:)
      real(kind = kreal), allocatable :: recv_psf(:)
      integer(kind = kint), allocatable :: isend_psf(:)
      integer(kind = kint), allocatable :: irecv_psf(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_psf_outputs_num(nprocs, my_rank, num_psf)
!
      integer(kind = kint), intent(in) :: num_psf, nprocs, my_rank
!
!
      if(my_rank .eq. 0) then
        allocate( psf_out(num_psf) )
      else
        allocate( psf_out(0) )
      end if
!
      allocate( istack_nod_output_psf(0:num_psf) )
      allocate( nnod_para_psf(num_psf*nprocs) )
      allocate( istack_nod_para_psf(0:num_psf*nprocs) )
      allocate( nnod_recv_psf(num_psf*nprocs) )
      allocate( istack_nod_recv_psf(0:num_psf*nprocs) )
!
      allocate( istack_ele_output_psf(0:num_psf) )
      allocate( nele_para_psf(num_psf*nprocs) )
      allocate( istack_ele_para_psf(0:num_psf*nprocs) )
      allocate( nele_recv_psf(num_psf*nprocs) )
      allocate( istack_ele_recv_psf(0:num_psf*nprocs) )
!
      nnod_para_psf = 0
      nnod_recv_psf = 0
      istack_nod_output_psf = 0
      istack_nod_para_psf = 0
      istack_nod_recv_psf = 0
!
      nele_para_psf = 0
      nele_recv_psf = 0
      istack_ele_output_psf = 0
      istack_ele_para_psf = 0
      istack_ele_recv_psf = 0
!
      end subroutine allocate_psf_outputs_num
!
! ----------------------------------------------------------------------
!
      subroutine allocate_psf_outputs_data(my_rank, num_psf)
!
      integer(kind=kint ) , intent(in) :: my_rank, num_psf
!
      integer(kind = kint) :: i_psf
!
!
      allocate( ihash_output_psf(ntot_nod_output_psf) )
      if(ntot_nod_output_psf .gt. 0) ihash_output_psf = 0
!
      if(my_rank .eq. 0) then
        do i_psf = 1, num_psf
          call alloc_ucd_node_t(psf_out(i_psf))
          call alloc_ucd_ele_t(psf_out(i_psf))
          call alloc_ucd_phys_data_t(psf_out(i_psf))
        end do
      end if
!
      end subroutine allocate_psf_outputs_data
!
! ----------------------------------------------------------------------
!
      subroutine allocate_SR_array_psf(my_rank, max_ncomp_psf_out,      &
     &          nnod_psf_tot, npatch_tot_psf_smp)
!
      integer(kind=kint ) , intent(in)   ::  my_rank
      integer(kind=kint ) , intent(in)   ::  max_ncomp_psf_out
      integer(kind=kint ) , intent(in)   ::  nnod_psf_tot
      integer(kind=kint ) , intent(in)   ::  npatch_tot_psf_smp
!
      integer(kind = kint) :: nmax_comp, nmax_int
!
!
      nmax_comp = max(max_ncomp_psf_out,num_triangle)
      allocate (send_psf(nmax_comp*nnod_psf_tot))
      if(nmax_comp*nnod_psf_tot .gt. 0) send_psf = 0.0d0
!
      nmax_int = max(num_triangle*npatch_tot_psf_smp,nnod_psf_tot)
      allocate (isend_psf(nmax_int))
      if(nmax_int .gt. 0) isend_psf = 0
!
      if (my_rank.eq.0) then
        allocate (recv_psf(nmax_comp*ntot_nod_output_psf))
        if(ntot_nod_output_psf .gt. 0) recv_psf = 0.0d0
!
        nmax_int                                                        &
     &     = max(num_triangle*ntot_ele_output_psf,ntot_nod_output_psf)
        allocate (irecv_psf(nmax_int))
        if(nmax_int .gt. 0) irecv_psf = 0
      end if
!
      end subroutine allocate_SR_array_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_psf_outputs_num
!
      deallocate( istack_nod_output_psf)
      deallocate( nnod_para_psf)
      deallocate( istack_nod_para_psf)
!
      deallocate( istack_ele_output_psf)
      deallocate( nele_para_psf)
      deallocate( istack_ele_para_psf)
!
      deallocate( psf_out )
!
      end subroutine deallocate_psf_outputs_num
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_psf_outputs_data(my_rank, num_psf)
!
      integer(kind = kint), intent(in) :: my_rank, num_psf
      integer(kind = kint) :: i_psf
!
!
      deallocate(ihash_output_psf)
!
      if(my_rank .eq. 0) then
        do i_psf = 1, num_psf
          call dealloc_ucd_phys_data_t(psf_out(i_psf))
          call dealloc_ucd_phys_name_t(psf_out(i_psf))
          call dealloc_ucd_ele_t(psf_out(i_psf))
          call dealloc_ucd_node_t(psf_out(i_psf))
        end do
      end if
!
      end subroutine deallocate_psf_outputs_data
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_SR_array_psf(my_rank)
!
      integer(kind=kint ) , intent(in)   ::  my_rank
!      number of domain
!
        deallocate (send_psf )
        deallocate (isend_psf )
!
      if (my_rank.eq.0) then
        deallocate (recv_psf)
        deallocate (irecv_psf)
      end if
!
      end subroutine deallocate_SR_array_psf
!
! ----------------------------------------------------------------------
!
      end module m_psf_outputs
