!
!      module take_avarages_4_psf
!
!      Written by H. Matsui
!
!      subroutine input_psf_result(itype_psf, psf_head, istep)
!      subroutine cal_rms_ave_4_psf
!      subroutine cal_minmax_psf
!
      module take_avarages_4_psf
!
      use m_precision
      use m_psf_results
      use m_norms_4_psf
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine input_psf_result(itype_psf, psf_head, istep)
!
      use m_field_file_format
      use read_psf_select_4_zlib
!
      integer(kind = kint), intent(in) :: itype_psf, istep
      character(len=kchara), intent(in) :: psf_head
!
!
      if(   mod(itype_psf,100)/10 .eq. iflag_udt/10) then
          call sel_read_psf_grid_file(itype_psf, psf_head)
          call sel_read_alloc_psf_udt_file(itype_psf, psf_head, istep)
      else
          call sel_read_alloc_psf_ucd_file(itype_psf, psf_head, istep)
      end if
!
      end subroutine input_psf_result
!
!-----------------------------------------------------------------------
!
      subroutine cal_rms_ave_4_psf
!
      integer(kind = kint) :: iele, icomp, i1, i2, i3
      real(kind = kreal) :: d_ele
!
      ave_psf = 0.0d0
      rms_psf = 0.0d0
      do icomp = 1, ncomptot_psf
        do iele = 1, numele_psf
!
          i1 = ie_psf(iele,1)
          i2 = ie_psf(iele,2)
          i3 = ie_psf(iele,3)
!
          d_ele = (d_nod_psf(i1,icomp) + d_nod_psf(i2,icomp)            &
     &           + d_nod_psf(i3,icomp) ) / 3.0d0
!
          rms_psf(icomp) = rms_psf(icomp)                               &
     &          + d_ele * d_ele * area_psf(iele)
          ave_psf(icomp) = ave_psf(icomp) + d_ele * area_psf(iele)
        end do
        rms_psf(icomp) = sqrt( rms_psf(icomp)/area_total_psf )
        ave_psf(icomp) = ave_psf(icomp)/area_total_psf
      end do
!
      end subroutine cal_rms_ave_4_psf
!
!-----------------------------------------------------------------------
!
      subroutine cal_minmax_psf
!
      integer(kind = kint) :: inod, icomp
!
      do icomp = 1, ncomptot_psf
        xmin_psf(icomp) = 1.0d30
        do inod = 1, numnod_psf
          xmin_psf(icomp) = min(xmin_psf(icomp), d_nod_psf(inod,icomp))
        end do
        xmax_psf(icomp) = xmin_psf(icomp)
        do inod = 1, numnod_psf
          xmax_psf(icomp) = max(xmax_psf(icomp), d_nod_psf(inod,icomp))
        end do
      end do
!
      end subroutine cal_minmax_psf
!
!-----------------------------------------------------------------------
!
      end module take_avarages_4_psf
