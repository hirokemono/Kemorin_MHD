!
!      module m_2nd_field_data_IO
!
!      Written by H. Matsui on Oct., 2007
!
!      subroutine allocate_field2_name_IO
!      subroutine allocate_field2_data_IO
!
!      subroutine deallocate_field2_name_IO
!      subroutine deallocate_field2_data_IO
!
!      subroutine cal_istack_fld2_comp_IO
!
      module m_2nd_field_data_IO
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_fld2_file = 15
!
      integer(kind = kint) :: iflag_field2_data_fmt =  0
!      integer(kind = kint) :: iflag_phys_header_def = 0
!
      character(len=kchara) :: field2_file_head
!      character(len=kchara) :: org_sph_spec_head
!      integer(kind = kint) ::  iflag_sph_spec_head =  0
!      integer(kind = kint) ::  iflag_org_sph_spec_head =  0
!
      integer(kind = kint) :: num_phys2_fld_IO
      integer(kind = kint) :: ntot_phys2_comp_IO
      integer(kind = kint), allocatable :: num_phys2_comp_IO(:)
      integer(kind = kint), allocatable :: istack_phys2_comp_IO(:)
!
      character(len=kchara), allocatable :: phys2_name_IO(:)
!
      integer(kind = kint) :: ngrid2_sph_IO
      real(kind = kreal), allocatable :: phys2_data_IO(:,:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_field2_name_IO
!
      allocate( num_phys2_comp_IO(num_phys2_fld_IO) )
      allocate( istack_phys2_comp_IO(0:num_phys2_fld_IO) )
      allocate( phys2_name_IO(num_phys2_fld_IO) )
      num_phys2_comp_IO = 0
      istack_phys2_comp_IO = -1
!
      end subroutine allocate_field2_name_IO
!
! -------------------------------------------------------------------
!
      subroutine allocate_field2_data_IO
!
      allocate( phys2_data_IO(ngrid2_sph_IO, ntot_phys2_comp_IO) )
      phys2_data_IO = 0.0d0
!
      end subroutine allocate_field2_data_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine deallocate_field2_name_IO
!
      deallocate( num_phys2_comp_IO )
      deallocate( istack_phys2_comp_IO )
      deallocate( phys2_name_IO )
!
      end subroutine deallocate_field2_name_IO
!
! -------------------------------------------------------------------
!
      subroutine deallocate_field2_data_IO
!
      deallocate( phys2_data_IO )
!
      end subroutine deallocate_field2_data_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cal_istack_fld2_comp_IO
!
      use m_constants
      use cal_minmax_and_stacks
!
!
      call s_cal_total_and_stacks(num_phys2_fld_IO, num_phys2_comp_IO,  &
     &    izero, istack_phys2_comp_IO, ntot_phys2_comp_IO)
!
      end subroutine cal_istack_fld2_comp_IO
!
! -------------------------------------------------------------------
!
      end module m_2nd_field_data_IO
