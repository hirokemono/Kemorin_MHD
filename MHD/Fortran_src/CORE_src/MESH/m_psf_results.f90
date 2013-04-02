!
!      module m_psf_results
!
!      Written by H. Matsui
!
!      subroutine allocate_psf_results
!      subroutine allocate_psf_num_field
!      subroutine allocate_psf_field_data
!
!      subroutine deallocate_psf_results
!
!      subroutine count_stack_tot_psf_field
!
!      subroutine write_headers_psf_comp_name(id_file)
!      subroutine check_headers_psf_comp_name(id_file, ierr)
!
      module m_psf_results
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: numnod_psf, numele_psf
      integer(kind = kint) :: nfield_psf, ncomptot_psf
!
      real(kind = kreal), allocatable :: xx_psf(:,:)
      real(kind = kreal), allocatable :: d_nod_psf(:,:)
      integer(kind = kint), allocatable :: inod_psf(:)
!
      integer(kind = kint), allocatable :: iele_psf(:)
      integer(kind = kint), allocatable :: ie_psf(:,:)
      integer(kind = kint), allocatable :: ncomp_psf(:)
      integer(kind = kint), allocatable :: istack_comp_psf(:)
      character(len=kchara), allocatable :: psf_data_name(:)
!
      real(kind = kreal), allocatable :: ave_psf(:), rms_psf(:)
      real(kind = kreal), allocatable :: xmin_psf(:), xmax_psf(:)
!
      integer(kind = kint), parameter :: id_psf_result = 7
!
      character(len=kchara) :: flag_psf
      integer(kind = kint) :: iflag_psf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_results
!
      allocate ( xx_psf(numnod_psf,3) )
      allocate ( inod_psf(numnod_psf) )
      allocate ( ie_psf(numele_psf,3) )
      allocate ( iele_psf(numele_psf) )
!
      if(numnod_psf .gt. 0) then
        xx_psf = 0.0d0
        inod_psf = 0
      end if
      if(numele_psf .gt. 0) then
        iele_psf =         0
        ie_psf =           0
      end if
!
      end subroutine allocate_psf_results
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_num_field
!
      allocate ( ncomp_psf(nfield_psf) )
      allocate ( istack_comp_psf(0:nfield_psf) )
      allocate ( psf_data_name(nfield_psf) )
!
      ncomp_psf =      -1
      istack_comp_psf = 0
!
      end subroutine allocate_psf_num_field
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_field_data
!
      allocate ( d_nod_psf(numnod_psf,ncomptot_psf) )
!
      allocate ( xmin_psf(ncomptot_psf) )
      allocate ( xmax_psf(ncomptot_psf) )
      allocate ( ave_psf(ncomptot_psf) )
      allocate ( rms_psf(ncomptot_psf) )
!
      d_nod_psf = 0.0d0
!
      xmin_psf = 1.0d30
      xmax_psf = 0.0d0
      ave_psf = 0.0d0
      rms_psf = 0.0d0
!
      end subroutine allocate_psf_field_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_stack_tot_psf_field
!
      integer(kind = kint) :: i
!
!
      istack_comp_psf(0) = 0
      do i = 1, nfield_psf
        istack_comp_psf(i) = istack_comp_psf(i-1) + ncomp_psf(i)
      end do
      ncomptot_psf = istack_comp_psf(nfield_psf)
!
      end subroutine count_stack_tot_psf_field
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_psf_results
!
      deallocate ( d_nod_psf )
      deallocate ( inod_psf, xx_psf )
      deallocate ( iele_psf, ie_psf )
      deallocate ( ncomp_psf )
      deallocate ( istack_comp_psf )
      deallocate ( psf_data_name )
!
      deallocate ( xmin_psf )
      deallocate ( xmax_psf )
      deallocate ( ave_psf )
      deallocate ( rms_psf )
!
      end subroutine deallocate_psf_results
!
!-----------------------------------------------------------------------
!
      subroutine write_headers_psf_comp_name(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      character(len=kchara) :: tmpchara
      integer(kind = kint) :: j, k
!
!
      do j = 1, nfield_psf
        if ( ncomp_psf(j) .eq. 1) then
          write(id_file,*) trim( psf_data_name(j) ), ', '
        else
          do k = 1, ncomp_psf(j)
            write(tmpchara,1000) trim( psf_data_name(j) ), k
            write(id_file,*) trim(tmpchara)
          end do
        end if
      end do
!
 1000 format(a,'_',i1,', ')
!
      end subroutine write_headers_psf_comp_name
!
!-----------------------------------------------------------------------
!
      subroutine check_headers_psf_comp_name(id_file, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: tmpchara, tmpchara2
      integer(kind = kint) :: j, k
!
!
      ierr = 0
      do j = 1, nfield_psf
        if ( ncomp_psf(j) .eq. 1) then
          read(id_file,*,end=101,err=101) tmpchara
          if( tmpchara .ne. psf_data_name(j) ) then
            ierr = 1
            exit
          end if
        else
          do k = 1, ncomp_psf(j)
            write(tmpchara2,1000) trim( psf_data_name(j) ), k
            read(id_file,*,end=101,err=101) tmpchara
            if( tmpchara .ne. tmpchara2 ) then
              ierr = 1
              exit
            end if
          end do
        end if
      end do
!
      return
 101  ierr = 2
      return
!
 1000 format(a,'_',i1)
!
      end subroutine check_headers_psf_comp_name
!
!-----------------------------------------------------------------------
!
      end module  m_psf_results
