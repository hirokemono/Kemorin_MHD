!m_sph_zonal_ene_spectr.f90
!      module m_sph_zonal_ene_spectr
!
      module m_sph_zonal_ene_spectr
!
!     Written by H. Matsui on Nov., 2007
!
      use m_precision
      use m_spheric_parameter
      use m_node_id_spherical_IO
      use m_sph_spectr_data
!
      implicit none
!
      integer(kind = kint) :: max_abs_m, min_abs_m
      integer(kind = kint) :: num_ene_spec
      integer(kind = kint) :: num_ene_mphi
      integer(kind = kint), allocatable :: inod_ene_gl_rtp(:,:)
      integer(kind = kint), allocatable :: itgt_zonal_ene_spec(:)
      real(kind = kreal), allocatable :: d_ene_spec(:,:)
      real(kind = kreal), allocatable :: d_tave_ene_spec(:,:)
!
      integer(kind = kint), parameter :: id_zene_spec_file = 18
      character(len=kchara) :: zene_file_name
!
      private :: id_zene_spec_file, zene_file_name
!
!      subroutine allocate_zonal_ene_spec_idx
!      subroutine allocate_zonal_ene_spec_data
!
!      subroutine write_zonal_ene_spec(my_rank, istep_fld)
!      subroutine write_tave_zonal_ene_spec(my_rank)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_zonal_ene_spec_idx
!
!
      allocate( itgt_zonal_ene_spec(nnod_rtp) )
      allocate( inod_ene_gl_rtp(num_ene_spec,3) )
      itgt_zonal_ene_spec = 0
      inod_ene_gl_rtp = 0
!
      end subroutine allocate_zonal_ene_spec_idx
!
! -------------------------------------------------------------------
!
      subroutine allocate_zonal_ene_spec_data
!
!
      allocate(d_ene_spec(ntot_phys_rtp,num_ene_spec))
      allocate(d_tave_ene_spec(ntot_phys_rtp,num_ene_spec))
      d_ene_spec =      0.0d0
      d_tave_ene_spec = 0.0d0
!
      end subroutine allocate_zonal_ene_spec_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine write_zonal_ene_spec(my_rank, istep_fld)
!
      use set_parallel_file_name
      use m_ctl_params_zonal_fft
!
      integer(kind = kint), intent(in) :: my_rank, istep_fld
      integer(kind = kint) :: i, j, ist, ied
      character(len=kchara) :: fname_tmp1, fname_tmp2
!
!
      call add_int_suffix(istep_fld, ene_spec_head, fname_tmp1)
      call add_int_suffix(my_rank, fname_tmp1, fname_tmp2)
      call add_dat_extension(fname_tmp2, zene_file_name)
!
      write(*,*) 'ascii data file: ', trim(zene_file_name)
      open(id_zene_spec_file, file = zene_file_name, form= 'formatted')
!
      write(id_zene_spec_file,'(2i15)') num_ene_spec, num_phys_rtp
      write(id_zene_spec_file,'(10i5)')                                 &
     &      num_phys_comp_rtp(1:num_phys_rtp)
!
      write(id_zene_spec_file,*) 'inod, kr, itheta, mphi'
      do j = 1, num_ene_spec
        write(id_zene_spec_file,'(4i10)')  j, inod_ene_gl_rtp(j,1:3)
      end do
!
!
      do i = 1, num_phys_rtp
!
        write(id_zene_spec_file,'(a)') trim(phys_name_rtp(i))
!
        ist = istack_phys_comp_rtp(i-1) + 1
        ied = istack_phys_comp_rtp(i)
        do j = 1, num_ene_spec
          write(id_zene_spec_file,'(1p20E25.15e3)')                     &
     &                d_ene_spec(ist:ied,j)
        end do
!
      end do
!
      close(id_zene_spec_file)
!
      end subroutine write_zonal_ene_spec
!
! -------------------------------------------------------------------
!
      subroutine write_tave_zonal_ene_spec(my_rank)
!
      use m_ctl_params_zonal_fft
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, j, ist, ied
      character(len=kchara) :: fname_tmp1, fname_tmp2
!
!
      call add_int_suffix(istep_start, tave_ene_spec_head, fname_tmp1)
      call add_int_suffix(istep_end, fname_tmp1, fname_tmp2)
      call add_int_suffix(my_rank, fname_tmp2, fname_tmp1)
      call add_dat_extension(fname_tmp1, zene_file_name)
!
      write(*,*) 'ascii data file: ', trim(zene_file_name)
      open(id_zene_spec_file, file = zene_file_name, form= 'formatted')
!
      write(id_zene_spec_file,'(2i15)') num_ene_spec, num_phys_rtp
      write(id_zene_spec_file,'(10i5)')                                 &
     &      num_phys_comp_rtp(1:num_phys_rtp)
!
      write(id_zene_spec_file,*) 'inod, kr, itheta, mphi'
      do j = 1, num_ene_spec
        write(id_zene_spec_file,'(4i10)')  j, inod_ene_gl_rtp(j,1:3)
      end do
!
!
      do i = 1, num_phys_rtp
!
        write(id_zene_spec_file,'(a)') trim(phys_name_rtp(i))
!
        ist = istack_phys_comp_rtp(i-1) + 1
        ied = istack_phys_comp_rtp(i)
        do j = 1, num_ene_spec
          write(id_zene_spec_file,'(1p20E25.15e3)')                     &
     &          d_tave_ene_spec(ist:ied,j)
        end do
!
      end do
!
      close(id_zene_spec_file)
!
      end subroutine write_tave_zonal_ene_spec
!
! -------------------------------------------------------------------
!
      end module m_sph_zonal_ene_spectr
