!m_flex_delta_t_data.f90
!     module m_flex_delta_t_data
!
!      Written by H. Matsui on Nov., 2009
!
!      subroutine allocate_check_delta_t_name
!      subroutine allocate_check_delta_t_rms
!      subroutine allocate_check_delta_t_data
!      subroutine deallocate_check_delta_t_rms
!      subroutine deallocate_check_delta_t_data
!
!      subroutine write_delta_t_check_head(id_file)
!      subroutine write_rms_delta_t_check(id_file, i_step, time)
!      subroutine write_max_delta_t_check(id_file, i_step, time)
!      subroutine write_min_delta_t_check(id_file, i_step, time)
!
!      subroutine read_rms_delta_t_check(id_file, i_step, time)
!      subroutine read_max_delta_t_check(id_file, i_step, time)
!      subroutine read_min_delta_t_check(id_file, i_step, time)
!
      module m_flex_delta_t_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      integer(kind = kint) :: nfld_dratio, ntot_dratio
      character(len=kchara), allocatable :: d_ratio_name(:)
      integer(kind = kint), allocatable :: ncomp_dratio(:)
      integer(kind = kint), allocatable :: istack_dratio(:)
!
      real(kind=kreal), allocatable :: d_ratio_min_smp(:,:)
      real(kind=kreal), allocatable :: d_ratio_max_smp(:,:)
      real(kind=kreal), allocatable :: d_ratio_min_l(:)
      real(kind=kreal), allocatable :: d_ratio_max_l(:)
      real(kind=kreal), allocatable :: d_ratio_min(:)
      real(kind=kreal), allocatable :: d_ratio_max(:)
!
      real(kind=kreal), allocatable :: rms_dt_local(:)
      real(kind=kreal), allocatable :: ave_dt_local(:)
      real(kind=kreal), allocatable :: rms_dt_global(:)
      real(kind=kreal), allocatable :: ave_dt_global(:)
      real(kind=kreal), allocatable :: rms_dt_pre1(:)
      real(kind=kreal), allocatable :: rms_dt_pre2(:)
      real(kind=kreal), allocatable :: d_ratio(:)
!
      integer(kind = kint), allocatable :: inod_min_dratio(:)
      integer(kind = kint), allocatable :: inod_max_dratio(:)
!
      real(kind=kreal) ::  d_ratio_allmax
      real(kind=kreal) ::  d_ratio_allmin
!
      integer(kind=kint) :: i_drmax_v = izero
      integer(kind=kint) :: i_drmax_p = izero
      integer(kind=kint) :: i_drmax_t = izero
      integer(kind=kint) :: i_drmax_b = izero
      integer(kind=kint) :: i_drmax_f = izero
      integer(kind=kint) :: i_drmax_d = izero
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_check_delta_t_name
!
      allocate( d_ratio_name(nfld_dratio) )
      allocate( ncomp_dratio(nfld_dratio) )
      allocate( istack_dratio(0:nfld_dratio) )
!
      if(nfld_dratio .gt. izero) ncomp_dratio = izero
      istack_dratio = izero
!
      end subroutine allocate_check_delta_t_name
!
! ----------------------------------------------------------------------
!
      subroutine allocate_check_delta_t_rms
!
!
      allocate(rms_dt_local(ntot_dratio))
      allocate(ave_dt_local(ntot_dratio))
      allocate(rms_dt_global(0:ntot_dratio))
      allocate(ave_dt_global(0:ntot_dratio))
      allocate(rms_dt_pre1(0:ntot_dratio))
      allocate(rms_dt_pre2(0:ntot_dratio))
      allocate(d_ratio(ntot_dratio))
!
      if(ntot_dratio .gt. 0) then
        rms_dt_local =  zero
        ave_dt_local =  zero
        rms_dt_global = zero
        ave_dt_global = zero
        rms_dt_pre1 =   zero
        rms_dt_pre2 =   zero
        d_ratio =       zero
      end if
!
      end subroutine allocate_check_delta_t_rms
!
! ----------------------------------------------------------------------
!
      subroutine allocate_check_delta_t_data
!
      use m_machine_parameter
!
!
      allocate(d_ratio_min(ntot_dratio))
      allocate(d_ratio_max(ntot_dratio))
      allocate(d_ratio_min_l(ntot_dratio))
      allocate(d_ratio_max_l(ntot_dratio))
      allocate(d_ratio_min_smp(np_smp,ntot_dratio))
      allocate(d_ratio_max_smp(np_smp,ntot_dratio))
!
      allocate(inod_min_dratio(ntot_dratio))
      allocate(inod_max_dratio(ntot_dratio))
!
      if(ntot_dratio .gt. 0) then
        d_ratio_min =     zero
        d_ratio_max =     zero
        d_ratio_min_l =   zero
        d_ratio_max_l =   zero
        d_ratio_min_smp = zero
        d_ratio_max_smp = zero
        inod_max_dratio = izero
        inod_min_dratio = izero
      end if
!
      end subroutine allocate_check_delta_t_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_check_delta_t_rms
!
!
      deallocate(rms_dt_local, rms_dt_global, rms_dt_pre1)
      deallocate(ave_dt_local, ave_dt_global, rms_dt_pre2)
      deallocate(d_ratio)
!
      end subroutine deallocate_check_delta_t_rms
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_check_delta_t_data
!
!
      deallocate(d_ratio_min, d_ratio_min_l, d_ratio_min_smp)
      deallocate(d_ratio_max, d_ratio_max_l, d_ratio_max_smp)
      deallocate( d_ratio_name, ncomp_dratio,  istack_dratio )
      deallocate( inod_min_dratio, inod_max_dratio)
!
      end subroutine deallocate_check_delta_t_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_delta_t_check_head(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i_fld
!
!
      write(id_file,'(a)') '! number of field and component to check'
      write(id_file,'(2i5)') nfld_dratio, ntot_dratio
!
      write(id_file,'(a)') 'step, '
      do i_fld = 1, nfld_dratio
        if(ncomp_dratio(i_fld) .eq. ithree) then
          write(id_file,'(a,a4)') trim(d_ratio_name(i_fld)), '_x, '
          write(id_file,'(a,a4)') trim(d_ratio_name(i_fld)), '_y, '
          write(id_file,'(a,a4)') trim(d_ratio_name(i_fld)), '_z, '
        else if (ncomp_dratio(i_fld) .eq. ione) then
          write(id_file,'(a,a2)') trim(d_ratio_name(i_fld)), ', '
        end if
      end do
!
      end subroutine write_delta_t_check_head
!
! ----------------------------------------------------------------------
!
      subroutine write_rms_delta_t_check(id_file, i_step, time)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) ::   time
!
      write(id_file,'(i16, 1p127E25.15e3)') i_step, time,               &
     &       d_ratio(1:ntot_dratio)
!
      end subroutine write_rms_delta_t_check
!
! ----------------------------------------------------------------------
!
      subroutine write_max_delta_t_check(id_file, i_step, time)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) ::   time
!
      write(id_file,'(i16, 1p127E25.15e3)') i_step, time,               &
     &       d_ratio_max(1:ntot_dratio)
!
      end subroutine write_max_delta_t_check
!
! ----------------------------------------------------------------------
!
      subroutine write_min_delta_t_check(id_file, i_step, time)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) ::   time
!
      write(id_file,'(i16, 1p127E25.15e3)') i_step, time,               &
     &       d_ratio_min(1:ntot_dratio)
!
      end subroutine write_min_delta_t_check
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_rms_delta_t_check(id_file, i_step, time)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) ::   time
!
!
      read(id_file,*) i_step, time, d_ratio(1:ntot_dratio)
!
      end subroutine read_rms_delta_t_check
!
! ----------------------------------------------------------------------
!
      subroutine read_max_delta_t_check(id_file, i_step, time)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) ::   time
!
!
      read(id_file,*) i_step, time, d_ratio_max(1:ntot_dratio)
!
      end subroutine read_max_delta_t_check
!
! ----------------------------------------------------------------------
!
      subroutine read_min_delta_t_check(id_file, i_step, time)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(inout) :: i_step
      real(kind = kreal), intent(inout) ::   time
!
!
      write(id_file,*) i_step, time, d_ratio_min(1:ntot_dratio)
!
      end subroutine read_min_delta_t_check
!
! ----------------------------------------------------------------------
!
      end module m_flex_delta_t_data
